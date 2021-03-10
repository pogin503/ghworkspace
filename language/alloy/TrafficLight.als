module TrafficLight

sig LightState {
  color: Light -> one Color
}

sig Light {}

abstract sig Color {}

one sig Red, Yellow, Green extends Color {}

sig Junction {
  lights: set Light
}

fact {
  -- 全ての状態で各交差点に赤を示す信号がある。
  all s: LightState, j: Junction |
    some so.color.Red & j.lights
}
