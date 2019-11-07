(module
  (func (export "add") (param $a i32) (param $b i32) (result i32)
        (local $sum i32) 
        block $outer (result i32) 
        block $inner (result i32) 
        local.get $a
        local.get $b
        i32.add
        local.set $sum
        local.get $sum
        br $outer
        end
        end
        ))
