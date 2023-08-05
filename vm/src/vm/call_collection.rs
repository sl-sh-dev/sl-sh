use crate::{GVm, Handle, VMError, VMResult, Value};

impl<ENV> GVm<ENV> {
    pub(crate) fn call_map(
        &mut self,
        handle: Handle,
        first_reg: u16,
        num_args: u16,
    ) -> VMResult<Value> {
        match num_args {
            1 => {
                let map = self.heap().get_map(handle);
                let res = if let Some(val) = map.get(&self.register(first_reg as usize + 1)) {
                    *val
                } else {
                    Value::Nil
                };
                Ok(res)
            }
            2 => {
                let map = self.heap().get_map(handle);
                let res = if let Some(val) = map.get(&self.register(first_reg as usize + 1)) {
                    *val
                } else {
                    self.register(first_reg as usize + 2)
                };
                Ok(res)
            }
            3 => {
                let eqi = self.intern("=");
                let key = self.register(first_reg as usize + 1);
                if matches!(key, Value::Undefined) {
                    return Err(VMError::new_vm("Key is undefined."));
                }
                let eq = self.register(first_reg as usize + 2);
                let val = self.register(first_reg as usize + 3);
                if let Value::Keyword(i) = eq {
                    if i == eqi {
                        let map = self.heap_mut().get_map_mut(handle)?;
                        let slot = map.entry(key);
                        slot.or_insert(val);
                        Ok(val)
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
        first_reg: u16,
        num_args: u16,
    ) -> VMResult<Value> {
        match num_args {
            1 => {
                let v = self.heap().get_vector(handle);
                let idx = self.register(first_reg as usize + 1).get_int(self)?;
                let idx = if idx >= 0 { idx } else { v.len() as i64 + idx };
                let res = if idx >= 0 {
                    if let Some(val) = v.get(idx as usize) {
                        *val
                    } else {
                        Value::Nil
                    }
                } else {
                    Value::Nil
                };
                Ok(res)
            }
            2 => {
                let v = self.heap().get_vector(handle);
                let idx = self.register(first_reg as usize + 1).get_int(self)?;
                let idx = if idx >= 0 { idx } else { v.len() as i64 + idx };
                let res = if idx >= 0 {
                    if let Some(val) = v.get(idx as usize) {
                        *val
                    } else {
                        self.register(first_reg as usize + 2)
                    }
                } else {
                    self.register(first_reg as usize + 2)
                };
                Ok(res)
            }
            3 => {
                let eqi = self.intern("=");
                let idx = self.register(first_reg as usize + 1).get_int(self)?;
                let idx = if idx >= 0 {
                    idx
                } else {
                    self.heap().get_vector(handle).len() as i64 + idx
                };
                if idx < 0 {
                    return Err(VMError::new_vm(
                        "Vector, index out of bounds (negative value to large).",
                    ));
                }
                let eq = self.register(first_reg as usize + 2);
                let val = self.register(first_reg as usize + 3);
                if let Value::Keyword(i) = eq {
                    if i == eqi {
                        let v = self.heap_mut().get_vector_mut(handle)?;
                        if let Some(slot) = v.get_mut(idx as usize) {
                            *slot = val;
                            Ok(Value::Vector(handle))
                        } else {
                            v.resize(idx as usize + 1, Value::Nil);
                            if let Some(slot) = v.get_mut(idx as usize) {
                                *slot = val;
                                Ok(Value::Vector(handle))
                            } else {
                                Err(VMError::new_vm(
                                    "Vector, index out of bounds (unable to grow vector).",
                                ))
                            }
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
        first_reg: u16,
        num_args: u16,
    ) -> VMResult<Value> {
        match num_args {
            1 => {
                let idx = self.register(first_reg as usize + 1).get_int(self)?;
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
                Ok(res)
            }
            _ => Err(VMError::new_vm("List wrong number of arguments.")),
        }
    }
}
