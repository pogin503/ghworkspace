(module
  (import "env" "log add message"
    (func $log add message (param i32 i32 i32)))
  (func (export "add message")
    (param $a i32) (param $b i32)
    (local $sum i32)
    local.get $a
    local.get $b
    i32.add
    local.set $sum
    (call $log add message
    (local.get $a) (local.get $b) (local.get $sum))
  )
)