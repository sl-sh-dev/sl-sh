use std::sync::Arc;

use crate::{mov_register, CallFrame, Chunk, Continuation, GVm, VMError, VMResult, Value};

/// Vm functions to handle runtime calling of anything callable.

impl<ENV> GVm<ENV> {
    /// Setup the rest (&) arguments for a callable.
    pub(crate) fn setup_rest(
        &mut self,
        chunk: &Arc<Chunk>,
        first_reg: u16,
        num_args: u16,
    ) -> (usize, Value) {
        let rest_reg = first_reg + chunk.args + chunk.opt_args;
        let v = if num_args < (chunk.args + chunk.opt_args) {
            Value::Nil
        } else {
            let rest_len = (num_args - (chunk.args + chunk.opt_args)) as usize + 1;
            let mut r = vec![Value::Undefined; rest_len];
            r.copy_from_slice(
                &self.register_slice()[rest_reg as usize..(rest_reg as usize + rest_len)],
            );
            self.alloc_list_ro(r)
        };
        (rest_reg.into(), v)
    }

    fn k_unshared_stack(&self, stack_top: usize, k: &Continuation) -> Option<(usize, &Vec<Value>)> {
        if !k.stack.is_empty() {
            if k.frame.stack_top >= stack_top {
                if let Value::CallFrame(h) = self.stack(stack_top) {
                    let frame = self.heap().get_callframe(h);
                    if let Value::CallFrame(k_h) = k.stack[stack_top] {
                        let k_frame = self.heap().get_callframe(k_h);
                        if frame.id != k_frame.id {
                            return Some((frame.stack_top, &frame.defers));
                        }
                    } else {
                        return Some((frame.stack_top, &frame.defers));
                    }
                }
            } else if let Value::CallFrame(h) = self.stack(stack_top) {
                let frame = self.heap().get_callframe(h);
                // If the continuation and frame have the same id then don't use the frame defers
                // (will lead to double deferring in some cases without this).
                return if k.frame.id == frame.id {
                    None
                } else {
                    Some((frame.stack_top, &frame.defers))
                };
            }
        }
        None
    }

    fn k_defers(&self, k: &Continuation) -> Option<usize> {
        if !self.defers.is_empty() {
            return None;
        }
        let mut stack_top = if let Some(k_stack_top) = self.k_stack_top {
            k_stack_top
        } else {
            self.stack_top
        };
        while let Some((next_stack_top, defers)) = self.k_unshared_stack(stack_top, k) {
            if stack_top == next_stack_top {
                break;
            }
            if !defers.is_empty() {
                return Some(stack_top);
            }
            stack_top = next_stack_top;
        }
        None
    }

    /// Build a call frame to be placed on the stack before transferring to a new chunk.
    pub(crate) fn make_call_frame(
        &mut self,
        chunk: Arc<Chunk>,
        called: Value,
        with_defers: bool,
    ) -> CallFrame {
        let defers = if with_defers {
            std::mem::take(&mut self.defers)
        } else {
            Vec::new()
        };
        let frame = CallFrame {
            id: self.callframe_id,
            chunk,
            ip: self.ip_ptr,
            current_ip: self.current_ip_ptr,
            stack_top: self.stack_top,
            this_fn: self.this_fn,
            defers,
            on_error: self.on_error,
            called,
        };
        self.callframe_id += 1;
        frame
    }

    fn finish_special_call(
        &mut self,
        chunk: Arc<Chunk>,
        tail_call: bool,
        first_reg: u16,
        res: Value,
    ) -> Arc<Chunk> {
        let res_reg = self.stack_top + first_reg as usize;
        if tail_call {
            // Go to last call frame so SRET does not mess up the return of a builtin or other special call.
            if let Some(frame) = self.call_frame() {
                let stack_top = frame.stack_top;
                let ip_ptr = frame.ip;
                let current_ip = frame.current_ip;
                let this_fn = frame.this_fn;
                let on_error = frame.on_error;
                let new_chunk = frame.chunk.clone();
                self.copy_frame_defers(); // Do this BEFORE we change stack_top...
                self.stack_top = stack_top;
                self.stack_max = self.stack_top + new_chunk.input_regs + new_chunk.extra_regs;
                self.ip_ptr = ip_ptr;
                self.current_ip_ptr = current_ip;
                self.this_fn = this_fn;
                self.on_error = on_error;
                *self.stack_mut(res_reg) = res;
                new_chunk
            } else {
                *self.stack_mut(res_reg) = res;
                chunk
            }
        } else {
            *self.stack_mut(res_reg) = res;
            chunk
        }
    }
    /// Main function to match and execute anything that is callable.
    pub fn make_call(
        &mut self,
        lambda: Value,
        chunk: Arc<Chunk>,
        first_reg: u16,
        num_args: u16,
        tail_call: bool,
    ) -> Result<Arc<Chunk>, (VMError, Arc<Chunk>)> {
        let mut do_cont = false;
        let result = match lambda {
            Value::Builtin(f_idx) => {
                let last_reg = (first_reg + num_args + 1) as usize;
                // Useful if the builtin runs bytecode that errors otherwise a waste...
                let frame = self.make_call_frame(chunk.clone(), lambda, false);
                let f = &self.buitins[f_idx as usize];
                let regs = self.register_slice();

                let res =
                    (f.func)(self, &regs[(first_reg + 1) as usize..last_reg]).map_err(|e| {
                        if self.err_frame().is_some() {
                            let call_frame = self.alloc_callframe(frame);
                            mov_register!(self, first_reg as usize, call_frame);
                            self.stack_top += first_reg as usize;
                        }
                        (e, chunk.clone())
                    })?;
                Ok(self.finish_special_call(chunk, tail_call, first_reg, res))
            }
            Value::Lambda(handle) => {
                let stack_top = self.stack_top;
                let l = self.heap().get_lambda(handle);
                check_num_args(&l, num_args).map_err(|e| (e, chunk.clone()))?;
                if !tail_call {
                    let frame = self.make_call_frame(chunk, lambda, true);
                    let aframe = self.alloc_callframe(frame);
                    mov_register!(self, first_reg as usize, aframe);
                    self.stack_top += first_reg as usize;
                }
                self.stack_max = self.stack_top + l.input_regs + l.extra_regs;
                self.this_fn = Some(lambda);
                self.ip_ptr = get_code!(l);
                if l.rest {
                    let (rest_reg, h) = self.setup_rest(&l, first_reg, num_args);
                    *self.stack_mut(stack_top + rest_reg) = h;
                }
                // XXX TODO- maybe test for stack overflow vs waiting for a panic.
                self.clear_opts(&l, first_reg, num_args);
                Ok(l)
            }
            Value::Closure(handle) => {
                let stack_top = self.stack_top;
                let (l, _) = self.heap().get_closure(handle);
                check_num_args(&l, num_args).map_err(|e| (e, chunk.clone()))?;
                let frame = if !tail_call {
                    let frame = self.make_call_frame(chunk, lambda, true);
                    self.stack_top += first_reg as usize;
                    Some(frame)
                } else {
                    assert_eq!(first_reg, 0);
                    None
                };
                // Take the heap so we can mutate self.  Put it back when done or will panic on next access.
                let heap = self.heap.take().expect("VM must have a Heap!");
                let caps = heap.get_closure_captures(handle);
                self.stack_max = self.stack_top + l.input_regs + l.extra_regs;
                self.this_fn = Some(lambda);
                self.ip_ptr = get_code!(l);
                if l.rest {
                    let (rest_reg, h) = self.setup_rest(&l, first_reg, num_args);
                    let cap_first = rest_reg + 1;
                    for (i, c) in caps.iter().enumerate() {
                        *self.stack_mut(stack_top + cap_first + i) = Value::Value(*c);
                    }
                    *self.stack_mut(stack_top + rest_reg) = h;
                } else {
                    let cap_first = (first_reg + l.args + l.opt_args + 1) as usize;
                    for (i, c) in caps.iter().enumerate() {
                        *self.stack_mut(stack_top + cap_first + i) = Value::Value(*c);
                    }
                }
                // Put the heap back, if this doesn't happen will panic on next access attempt.
                self.heap = Some(heap);
                if let Some(frame) = frame {
                    let aframe = self.alloc_callframe(frame);
                    *self.stack_mut(stack_top + first_reg as usize) = aframe;
                }
                self.clear_opts(&l, first_reg, num_args);
                Ok(l)
            }
            Value::Continuation(handle) => {
                let k = self.heap().get_continuation(handle);
                if num_args != 1 {
                    return Err((
                        VMError::new_vm(format!(
                            "Continuation takes one argument, supplied {num_args}."
                        )),
                        chunk,
                    ));
                }
                let from = self.k_defers(k);
                if let Some(from) = from {
                    let frame = self.call_frame_idx(from).expect("Invalid frame index!");
                    // Need to break the call frame lifetime from self to avoid extra work (allocations).
                    // This should safe because the stack and heap are not touched so the reference is
                    // stable.  The unwrap() is OK because the frame can not be NULL.
                    let frame: &CallFrame =
                        unsafe { (frame as *const CallFrame).as_ref().unwrap() };
                    self.defers.resize(frame.defers.len(), Value::Undefined);
                    self.defers.copy_from_slice(&frame.defers[..]);
                    self.k_stack_top = Some(frame.stack_top);
                }
                if let Some(defer) = self.defers.pop() {
                    let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                    self.ip_ptr = self.current_ip_ptr;
                    self.make_call(defer, chunk, first_reg, 0, false)
                } else {
                    self.k_stack_top = None;
                    do_cont = true;
                    Ok(chunk)
                }
            }
            Value::Map(handle) => {
                let res = self
                    .call_map(handle, first_reg, num_args)
                    .map_err(|e| (e, chunk.clone()))?;
                Ok(self.finish_special_call(chunk, tail_call, first_reg, res))
            }
            Value::Vector(handle) => {
                let res = self
                    .call_vector(handle, first_reg, num_args)
                    .map_err(|e| (e, chunk.clone()))?;
                Ok(self.finish_special_call(chunk, tail_call, first_reg, res))
            }
            Value::Pair(_) | Value::List(_, _) => {
                let res = self
                    .call_list(lambda, first_reg, num_args)
                    .map_err(|e| (e, chunk.clone()))?;
                Ok(self.finish_special_call(chunk, tail_call, first_reg, res))
            }
            Value::Value(handle) => {
                // Need to deref.
                self.make_call(
                    self.get_value(handle),
                    chunk,
                    first_reg,
                    num_args,
                    tail_call,
                )
            }
            _ => Err((
                VMError::new_vm(format!("CALL: Not a callable {lambda:?}.")),
                chunk,
            )),
        };
        if do_cont {
            // Had to break this out for continuations. Handling defers makes this necessary.
            match lambda {
                Value::Continuation(h) => {
                    // Take the heap so we can mutate self.  Put it back when down or will panic on next access.
                    let heap = self.heap.take().expect("VM must have a Heap!");
                    let k = heap.get_continuation(h);
                    let arg = self.register(first_reg as usize + 1);

                    self.defers.resize(k.frame.defers.len(), Value::Undefined);
                    self.defers.copy_from_slice(&k.frame.defers[..]);

                    self.stack_slice_mut()[..k.stack.len()].copy_from_slice(&k.stack[..]);
                    *self.stack_mut(k.arg_reg) = arg;
                    self.stack_top = k.frame.stack_top;
                    self.stack_max =
                        self.stack_top + k.frame.chunk.input_regs + k.frame.chunk.extra_regs;
                    self.ip_ptr = k.frame.ip;
                    self.current_ip_ptr = k.frame.current_ip;
                    self.this_fn = k.frame.this_fn;
                    self.on_error = k.frame.on_error;
                    let chunk = k.frame.chunk.clone();
                    // Put the heap back, if this doesn't happen will panic on next access attempt.
                    self.heap = Some(heap);
                    Ok(chunk)
                }
                _ => panic!("Must be a continuation!"),
            }
        } else {
            result
        }
    }

    /// Clear out the unused optional regs.
    /// Will clear working set to avoid writing to globals or closures by accident.
    fn clear_opts(&mut self, l: &Chunk, first_reg: u16, num_args: u16) {
        // First clear any optional arguments.
        let num_args = if l.rest && num_args == 0 {
            // Always have at least 1 arg if we have a rest argument.
            1
        } else {
            num_args
        };
        let end_arg = if l.rest {
            // Do not clear the rest arg.
            l.args + l.opt_args - 1
        } else {
            l.args + l.opt_args
        };
        if num_args < end_arg {
            for r in num_args..end_arg {
                mov_register!(
                    self,
                    first_reg as usize + (r + 1) as usize,
                    Value::Undefined
                );
            }
        }
        // Clear extra regs so things like closures or globals don't get changed by mistake.
        if l.extra_regs > 0 {
            for r in l.input_regs..=l.input_regs + l.extra_regs {
                mov_register!(self, first_reg as usize + r, Value::Undefined);
            }
        }
    }
}

/// Verify the number of args provided will work with a chunk.
fn check_num_args(l: &Chunk, num_args: u16) -> VMResult<()> {
    if l.rest {
        if num_args < (l.args - 1) {
            return Err(VMError::new_vm(format!(
                "To few arguments, expected at least {} got {}.",
                l.args - 1,
                num_args
            )));
        }
    } else {
        if num_args < l.args {
            return Err(VMError::new_vm(format!(
                "To few arguments, expected at least {} got {}.",
                l.args, num_args
            )));
        }
        if num_args > (l.args + l.opt_args) {
            return Err(VMError::new_vm(format!(
                "To many arguments, expected no more than {} got {}.",
                (l.args + l.opt_args),
                num_args
            )));
        }
    }
    Ok(())
}
