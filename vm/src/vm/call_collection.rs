use crate::{GVm, Handle, VMError, VMResult, Value};

impl<ENV> GVm<ENV> {
    pub(crate) fn call_map(
        &mut self,
        handle: Handle,
        registers: &mut [Value],
        first_reg: u16,
        num_args: u16,
    ) -> VMResult<()> {
        match num_args {
            1 => {
                let map = self.heap.get_map(handle);
                let res = if let Some(val) = map.get(&registers[first_reg as usize + 1]) {
                    *val
                } else {
                    Value::Nil
                };
                let res_reg = self.stack_top + first_reg as usize;
                self.stack[res_reg] = res;
                Ok(())
            }
            2 => {
                let map = self.heap.get_map(handle);
                let res = if let Some(val) = map.get(&registers[first_reg as usize + 1]) {
                    *val
                } else {
                    registers[first_reg as usize + 2]
                };
                let res_reg = self.stack_top + first_reg as usize;
                self.stack[res_reg] = res;
                Ok(())
            }
            3 => {
                let eqi = self.intern("=");
                let map = self.heap.get_map_mut(handle)?;
                let key = registers[first_reg as usize + 1];
                if matches!(key, Value::Undefined) {
                    return Err(VMError::new_vm("Key is undefined."));
                }
                let eq = registers[first_reg as usize + 2];
                let val = registers[first_reg as usize + 3];
                if let Value::Keyword(i) = eq {
                    if i == eqi {
                        let slot = map.entry(key);
                        slot.or_insert(val);
                        let res_reg = self.stack_top + first_reg as usize;
                        self.stack[res_reg] = val;
                        Ok(())
                    } else {
                        Err(VMError::new_vm(
                            "Map invalid second argument (expected :=).",
                        ))
                    }
                } else {
                    Err(VMError::new_vm(
                        "Map invalid second argument (expected :=).",
                    ))
                }
            }
            _ => Err(VMError::new_vm("Map wrong number of arguments.")),
        }
    }

    pub(crate) fn call_vector(
        &mut self,
        handle: Handle,
        registers: &mut [Value],
        first_reg: u16,
        num_args: u16,
    ) -> VMResult<()> {
        match num_args {
            1 => {
                let v = self.heap.get_vector(handle);
                let idx = registers[first_reg as usize + 1].get_int(self)?;
                let res = if idx >= 0 {
                    if let Some(val) = v.get(idx as usize) {
                        *val
                    } else {
                        return Err(VMError::new_vm("Vector, index out of bounds."));
                    }
                } else {
                    return Err(VMError::new_vm("A vector requires a positive index."));
                };
                let res_reg = self.stack_top + first_reg as usize;
                self.stack[res_reg] = res;
                Ok(())
            }
            3 => {
                let eqi = self.intern("=");
                let idx = registers[first_reg as usize + 1].get_int(self)?;
                let v = self.heap.get_vector_mut(handle)?;
                if idx < 0 {
                    return Err(VMError::new_vm("A vector requires a positive index."));
                }
                let eq = registers[first_reg as usize + 2];
                let val = registers[first_reg as usize + 3];
                if let Value::Keyword(i) = eq {
                    if i == eqi {
                        if let Some(slot) = v.get_mut(idx as usize) {
                            *slot = val;
                            Ok(())
                        } else {
                            Err(VMError::new_vm("Vector, index out of bounds."))
                        }
                    } else {
                        Err(VMError::new_vm(
                            "Vector invalid second argument (expected :=).",
                        ))
                    }
                } else {
                    Err(VMError::new_vm(
                        "Vector invalid second argument (expected :=).",
                    ))
                }
            }
            _ => Err(VMError::new_vm("Vector wrong number of arguments.")),
        }
    }

    pub(crate) fn call_list(
        &mut self,
        head: Value,
        registers: &mut [Value],
        first_reg: u16,
        num_args: u16,
    ) -> VMResult<()> {
        match num_args {
            1 => {
                let idx = registers[first_reg as usize + 1].get_int(self)?;
                let res = if idx >= 0 {
                    if let Some((mut car_out, mut cdr)) = head.get_pair(self) {
                        for _ in 0..idx {
                            if let Some((car, cdr_in)) = cdr.get_pair(self) {
                                car_out = car;
                                cdr = cdr_in;
                            } else {
                                return Err(VMError::new_vm("list: invalid index."));
                            }
                        }
                        car_out
                    } else {
                        return Err(VMError::new_vm("Not a list."));
                    }
                } else {
                    return Err(VMError::new_vm("A list requires a positive index."));
                };
                let res_reg = self.stack_top + first_reg as usize;
                self.stack[res_reg] = res;
                Ok(())
            }
            _ => Err(VMError::new_vm("Vector wrong number of arguments.")),
        }
    }
}
