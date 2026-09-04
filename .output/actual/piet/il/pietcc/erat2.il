; --- PIET ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_1
block_1:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_0
block_2:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_1
block_3:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_2
block_4:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_3
block_5:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_6
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_4
block_6:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_7
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_7
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_5
block_7:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_6
block_8:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_7
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_7
block_9:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_10
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_8
block_10:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_9
block_11:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_10
block_12:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
block_13:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_12
block_14:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 28
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_18
block_15:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_14
block_16:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_15
block_17:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_16
block_18:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_14
block_19:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_18
block_20:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_19
block_21:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_22
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_22
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_20
block_22:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_23
block_23:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_24
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_22
block_24:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_25
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_23
block_25:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_24
block_26:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 24
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_27
block_27:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_43
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_43
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_26
block_43:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_48
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_48
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_43
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_43
block_44:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_48
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_48
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_48
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_48
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_44
block_48:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_71
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_43
block_49:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_72
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_44
block_50:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_52
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_52
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_72
block_52:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_53
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_53
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_50
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_50
block_53:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_54
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_54
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_52
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_52
block_54:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_55
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_55
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_53
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_53
block_55:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_56
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_56
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_54
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_54
block_56:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 16
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_57
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 16
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_57
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_55
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_55
block_57:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_58
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_58
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_56
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_56
block_58:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_59
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_59
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_57
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_57
block_59:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_60
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_60
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_58
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_58
block_60:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 16
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 16
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_59
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_59
block_61:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_62
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_62
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_60
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_60
block_62:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_63
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_63
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_61
block_63:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_64
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_62
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_62
block_64:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_63
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_63
block_65:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_66
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_64
block_66:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_67
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_67
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_65
block_67:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_68
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_68
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_67
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_66
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_67
block_68:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_69
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_67
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_67
block_69:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_70
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_68
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_68
block_70:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_80
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_80
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_69
block_71:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_48
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_48
block_72:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_78
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_49
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_50
block_77:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_82
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_82
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_71
block_78:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_98
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_98
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_72
block_80:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 16
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_121
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_70
block_81:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_122
block_82:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_81
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_81
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_81
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_81
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_98:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_120
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_120
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_78
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_78
block_120:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_139
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_139
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_98
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_98
block_121:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_156
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_156
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_80
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_80
block_122:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_157
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_81
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_81
block_139:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_164
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_164
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_120
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_120
block_156:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_166
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_166
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_121
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_121
block_157:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_168
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_168
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
block_164:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_213
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_213
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_139
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_139
block_165:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_214
block_166:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_165
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_165
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_165
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_165
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_156
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_156
block_167:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 16
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_216
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_215
block_168:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_167
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_167
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_157
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_157
block_213:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_235
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_235
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_164
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_164
block_214:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_254
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_165
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_165
block_215:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 28
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_350
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_167
block_216:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_255
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_255
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_215
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_167
block_235:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_264
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_264
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_213
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_213
block_254:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_265
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_265
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_214
block_255:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_266
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_266
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_215
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_216
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_216
block_264:
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 20
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_235
block_265:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_301
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_301
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_254
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_254
block_266:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_215
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_215
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_255
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_255
block_301:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_310
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_310
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_265
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_265
block_309:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_264
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_264
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_329
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_329
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_264
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_264
block_310:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_348
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_348
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_301
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_301
block_329:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_359
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_359
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_309
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_309
block_348:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_361
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_360
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_310
block_350:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_373
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_373
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_215
block_359:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_374
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_374
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_329
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_329
block_360:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_67
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_67
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_348
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_348
block_361:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_378
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_378
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_348
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_348
block_373:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_376
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_376
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_350
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_350
block_374:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_380
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 16
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_359
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 16
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_359
block_375:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 36
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_385
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 36
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_385
block_376:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_379
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_379
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_373
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_373
block_377:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_375
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_375
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_381
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_381
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_375
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_375
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_375
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_375
block_378:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_387
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_387
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_361
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_361
block_379:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_383
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_383
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_376
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_376
block_380:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_409
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_409
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_374
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_374
block_381:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_386
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_386
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_377
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_377
block_382:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_406
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_406
block_383:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_388
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_388
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_379
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_379
block_385:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_375
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_375
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 20
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_394
block_386:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_390
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_390
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_381
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_381
block_387:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_382
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_382
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_382
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_382
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_378
block_388:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_391
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_391
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_383
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_383
block_389:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_392
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_389
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_380
block_390:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_395
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_395
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_386
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_386
block_391:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_408
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_408
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_388
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_388
block_392:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_393
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_393
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_389
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_389
block_393:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_394
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_394
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_392
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_392
block_394:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_385
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_385
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_393
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_393
block_395:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_410
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_410
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_390
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_390
block_396:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_413
block_397:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_398
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_398
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_396
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_396
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_396
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_396
block_398:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_399
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_399
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_397
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_397
block_399:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_400
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_400
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_398
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_398
block_400:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_401
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_401
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_399
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_399
block_401:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_402
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_402
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_400
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_400
block_402:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_403
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_403
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_401
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_401
block_403:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_404
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_404
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_402
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_402
block_404:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_405
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_405
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_403
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_403
block_405:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_406
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_406
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_404
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_404
block_406:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_405
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_405
block_407:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_415
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_415
block_408:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_407
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_407
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_407
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_407
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_391
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_391
block_409:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_411
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_411
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_380
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_380
block_410:
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_395
block_411:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_416
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_416
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_409
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_409
block_412:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 32
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_410
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_422
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_422
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 32
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_410
block_413:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_423
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_396
block_414:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_414
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_414
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_424
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_424
block_415:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_425
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_407
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_407
block_416:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_426
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_426
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_411
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_411
block_417:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_418
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_418
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_427
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_427
block_418:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_419
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_419
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_417
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_417
block_419:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_420
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_420
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_418
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_418
block_420:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_421
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_421
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_419
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_419
block_421:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_422
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_422
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_420
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_420
block_422:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_412
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_412
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_421
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_421
block_423:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_428
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_428
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_413
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_413
block_424:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_429
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_429
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_414
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_414
block_425:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_430
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_430
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_415
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_415
block_426:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_431
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_431
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_416
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_416
block_427:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_432
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_417
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_417
block_428:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_443
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_443
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_423
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_423
block_429:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_445
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_424
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_424
block_430:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_446
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_446
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_425
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_425
block_431:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_448
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_448
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_426
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_426
block_432:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_433
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_433
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_427
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_427
block_433:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_434
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_434
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_432
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_432
block_434:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_435
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_435
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_433
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_433
block_435:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_436
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_436
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_434
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_434
block_436:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_437
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_437
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_435
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_435
block_437:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_438
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_438
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_436
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_436
block_438:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_439
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_439
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_437
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_437
block_439:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_440
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_440
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_438
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_438
block_440:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_441
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_441
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_439
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_439
block_441:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_442
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_442
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_440
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_440
block_442:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_443
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_443
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_441
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_441
block_443:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_442
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_428
block_445:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_465
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_463
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_429
block_446:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_447
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_447
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_430
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_430
block_447:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_451
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_451
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_446
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_446
block_448:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_452
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_452
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_431
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_431
block_449:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_472
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_471
block_450:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_474
block_451:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_450
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_450
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_450
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_450
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_447
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_447
block_452:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_453
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_453
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_448
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_448
block_453:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_454
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_454
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_452
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_452
block_454:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_455
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_455
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_453
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_453
block_455:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_456
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_456
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_454
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_454
block_456:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_457
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_457
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_455
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_455
block_457:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_458
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_458
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_456
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_456
block_458:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_459
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_459
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_457
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_457
block_459:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_460
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_460
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_458
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_458
block_460:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_461
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_461
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_459
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_459
block_461:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_462
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_462
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_460
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_460
block_462:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_463
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_463
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_461
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_461
block_463:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_445
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_445
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_462
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_462
block_465:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_466
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_466
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_445
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_445
block_466:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_467
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_467
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_465
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_465
block_467:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_468
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_468
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_466
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_466
block_468:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_469
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_469
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_467
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_467
block_469:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_470
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_470
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_468
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_468
block_470:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_471
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_471
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_469
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_469
block_471:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_449
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_449
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_470
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_470
block_472:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_473
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_473
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_449
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_449
block_473:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_474
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_474
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_472
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_472
block_474:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_450
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_450
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_473
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_473
