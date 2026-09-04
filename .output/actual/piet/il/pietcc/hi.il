; --- PIET ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 9
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 9
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_1
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_8
block_1:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_0
block_3:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_1
block_5:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_6
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_10
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_1
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_3
block_6:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_7
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_7
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_3
block_7:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_6
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_3
block_8:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_1
block_10:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
block_11:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_23
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 10
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 10
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_6
block_12:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_7
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_7
block_13:
block_14:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_13
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_10
block_15:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_13
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_11
block_16:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_11
block_17:
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
block_19:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_16
block_20:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_23
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_19
block_23:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_20
