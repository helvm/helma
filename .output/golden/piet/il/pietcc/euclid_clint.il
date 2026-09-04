; --- PIET ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_3
block_3:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_0
block_4:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_11
block_5:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_6
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_3
block_6:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_4
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_4
block_8:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_6
block_9:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_8
block_11:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_4
block_12:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_23
block_14:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_11
block_15:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_24
block_16:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_15
block_19:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_22
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_14
block_21:
block_22:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_24
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_19
block_23:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_12
block_24:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_15
