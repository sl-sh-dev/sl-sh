use crate::{GVm, Value, VMError, VMResult, Handle};

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
