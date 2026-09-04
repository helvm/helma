; --- PIET ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_19
block_2:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_25
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_0
block_3:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_2
block_4:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_27
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_27
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
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_4
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
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_29
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
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_30
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_30
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_6
block_8:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_9
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_31
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_7
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_7
block_9:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_10
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_32
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_32
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_8
block_10:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_9
block_11:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_34
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_10
block_12:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_35
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_35
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
block_13:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_47
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_47
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_12
block_14:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_15
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_48
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_48
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_13
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_13
block_15:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_49
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_49
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_14
block_16:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_50
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_50
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_15
block_17:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_16
block_18:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_52
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_51
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_35
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_35
block_19:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_36
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_54
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_53
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_0
block_20:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_19
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_0
block_21:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_22
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_55
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_55
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_20
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
block_22:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_23
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_56
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_56
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
block_23:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_24
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_57
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_57
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_22
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
block_24:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_25
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_58
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_58
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_23
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
block_25:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_36
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_24
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_2
block_26:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_27
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_27
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_25
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_3
block_27:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_38
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_38
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_4
block_28:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_29
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_39
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_27
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_27
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
block_29:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_30
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_30
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_40
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_40
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_6
block_30:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_31
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_41
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_41
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_29
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_7
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_7
block_31:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_32
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_32
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_42
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_42
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_30
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_30
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
block_32:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_43
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_43
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_31
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_9
block_33:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_34
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_32
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_32
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_10
block_34:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_35
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_35
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_45
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_45
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_11
block_35:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_46
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_46
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_34
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_12
block_36:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_59
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_59
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_19
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_25
block_37:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_38
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_38
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_60
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_60
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_36
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_26
block_38:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_39
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_27
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_27
block_39:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_40
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_40
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_62
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_62
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_38
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_38
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
block_40:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_41
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_41
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_63
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_63
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_39
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_29
block_41:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_42
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_42
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_64
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_40
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_40
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_30
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_30
block_42:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_43
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_43
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_41
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_41
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_31
block_43:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_66
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_42
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_42
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_32
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_32
block_44:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_45
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_45
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_67
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_67
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_43
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_43
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_33
block_45:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_46
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_46
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_68
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_68
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_34
block_46:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_47
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_47
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_69
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_45
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_45
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_35
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_35
block_47:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_48
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_48
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_81
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_81
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_46
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_46
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_13
block_48:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_49
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_49
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_82
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_82
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_47
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_47
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_14
block_49:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_50
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_50
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_83
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_83
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_48
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_48
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_15
block_50:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_51
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_51
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_84
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_84
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_49
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_49
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_16
block_51:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_52
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_52
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_52
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_52
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_50
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_50
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_18
block_52:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_85
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_69
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_18
block_53:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_70
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_88
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_87
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_19
block_54:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_55
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_55
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_53
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_53
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_53
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_53
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_19
block_55:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_56
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_56
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_89
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_89
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_54
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_54
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
block_56:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_57
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_57
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_90
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_90
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_55
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_55
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_22
block_57:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_58
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_58
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_91
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_91
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_56
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_56
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_23
block_58:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_59
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_59
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_92
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_92
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_57
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_57
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_24
block_59:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_60
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_60
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_70
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_58
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_58
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_36
block_60:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_71
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_59
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_59
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_37
block_61:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_62
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_62
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_72
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_60
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_60
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_38
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_38
block_62:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_63
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_63
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_73
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_73
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_39
block_63:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_64
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_74
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_74
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_62
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_62
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_40
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_40
block_64:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_75
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_75
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_63
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_63
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_41
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_41
block_65:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_66
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_76
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_76
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_64
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_42
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_42
block_66:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_67
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_67
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_43
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_43
block_67:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_68
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_68
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_78
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_78
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_66
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_44
block_68:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_69
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_79
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_79
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_67
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_67
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_45
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_45
block_69:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_52
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_52
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_80
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_80
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_68
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_68
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_46
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_46
block_70:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_71
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_53
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_53
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_59
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_59
block_71:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_72
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_94
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_94
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_70
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_60
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_60
block_72:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_73
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_73
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_95
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_95
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_71
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_61
block_73:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_74
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_74
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_96
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_96
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_72
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_62
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_62
block_74:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_75
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_75
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_97
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_97
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_73
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_73
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_63
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_63
block_75:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_76
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_76
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_98
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_98
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_74
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_74
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_64
block_76:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_99
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_99
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_75
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_75
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_65
block_77:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_78
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_78
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_100
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_100
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_76
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_76
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_66
block_78:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_79
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_79
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_101
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_101
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_67
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_67
block_79:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_80
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_80
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_102
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_102
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_78
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_78
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_68
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_68
block_80:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_81
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_81
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_103
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_103
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_69
block_81:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_82
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_82
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_115
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_115
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_80
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_80
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_47
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_47
block_82:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_83
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_83
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_116
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_116
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_81
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_81
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_48
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_48
block_83:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_84
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_84
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_117
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_117
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_82
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_82
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_49
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_49
block_84:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_85
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_85
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_118
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_118
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_83
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_83
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_50
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_50
block_85:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_86
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_86
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_84
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_84
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_52
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_52
block_86:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_120
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_119
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_103
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_103
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_52
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_52
block_87:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_104
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_121
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_53
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_53
block_88:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_89
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_89
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_87
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_87
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_87
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_87
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_53
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_53
block_89:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_90
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_90
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_123
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_123
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_88
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_88
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_55
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_55
block_90:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_91
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_91
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_124
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_124
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_89
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_89
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_56
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_56
block_91:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_92
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_92
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_125
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_125
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_90
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_90
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
block_92:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_93
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_93
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_126
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_126
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_91
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_91
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_58
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_58
block_93:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_94
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_94
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_104
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_92
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_92
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_70
block_94:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_95
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_95
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_105
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_105
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_93
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_93
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_71
block_95:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_96
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_96
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_106
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_106
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_94
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_94
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_72
block_96:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_97
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_97
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_107
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_107
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_95
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_95
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_73
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_73
block_97:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_98
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_98
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_108
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_108
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_96
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_96
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_74
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_74
block_98:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_99
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_99
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_109
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_109
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_97
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_97
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_75
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_75
block_99:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_100
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_100
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_110
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_110
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_98
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_98
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_76
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_76
block_100:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_101
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_101
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_111
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_99
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_99
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_101:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_102
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_102
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_112
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_112
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_100
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_100
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_78
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_78
block_102:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_103
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_103
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_101
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_101
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_79
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_79
block_103:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_86
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_114
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_114
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_102
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_102
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_80
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_80
block_104:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_105
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_105
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_127
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_127
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_87
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_87
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_93
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_93
block_105:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_106
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_106
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_128
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_128
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_104
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_94
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_94
block_106:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_107
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_107
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_129
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_129
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_105
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_105
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_95
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_95
block_107:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_108
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_108
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_130
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_130
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_106
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_106
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_96
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_96
block_108:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_109
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_109
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_131
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_131
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_107
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_107
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_97
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_97
block_109:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_110
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_110
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_132
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_132
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_108
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_108
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_98
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_98
block_110:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_111
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_133
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_133
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_109
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_109
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_99
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_99
block_111:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_112
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_112
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_134
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_134
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_110
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_110
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_100
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_100
block_112:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_113
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_135
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_135
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_111
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_101
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_101
block_113:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_114
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_114
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_136
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_136
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_112
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_112
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_102
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_102
block_114:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_115
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_115
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_137
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_137
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_113
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_103
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_103
block_115:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_116
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_116
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_149
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_149
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_114
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_114
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_81
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_81
block_116:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_117
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_117
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_150
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_115
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_115
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_82
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_82
block_117:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_118
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_118
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_151
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_151
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_116
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_116
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_83
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_83
block_118:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_119
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_119
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_152
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_152
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_117
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_117
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_84
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_84
block_119:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_120
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_120
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_120
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_120
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_118
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_118
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_86
block_120:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_154
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_153
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_137
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_137
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_86
block_121:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_138
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_138
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_156
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_155
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_87
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_87
block_122:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_123
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_123
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_121
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_121
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_121
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_121
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_87
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_87
block_123:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_124
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_124
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_157
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_157
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_89
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_89
block_124:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_125
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_125
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_158
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_158
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_123
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_123
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_90
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_90
block_125:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_126
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_126
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_159
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_159
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_124
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_124
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_91
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_91
block_126:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_127
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_127
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_160
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_160
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_125
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_125
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_92
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_92
block_127:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_128
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_128
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_138
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_138
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_126
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_126
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_104
block_128:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_129
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_129
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_139
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_139
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_127
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_127
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_105
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_105
block_129:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_130
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_130
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_140
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_140
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_128
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_128
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_106
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_106
block_130:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_131
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_131
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_141
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_141
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_129
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_129
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_107
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_107
block_131:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_132
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_132
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_142
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_142
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_130
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_130
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_108
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_108
block_132:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_133
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_133
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_143
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_143
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_131
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_131
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_109
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_109
block_133:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_134
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_134
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_144
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_144
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_132
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_132
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_110
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_110
block_134:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_135
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_135
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_145
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_145
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_133
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_133
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_111
block_135:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_136
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_136
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_146
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_146
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_134
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_134
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_112
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_112
block_136:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_137
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_137
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_147
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_147
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_135
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_135
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_113
block_137:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_120
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_120
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_148
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_148
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_136
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_136
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_114
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
block_138:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_139
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_139
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_161
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_161
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_121
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_121
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_127
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_127
block_139:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_140
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_140
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_162
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_162
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_138
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_138
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_128
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_128
block_140:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_141
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_141
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_163
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_163
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_139
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_139
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_129
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_129
block_141:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_142
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_142
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_164
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_164
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_140
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_140
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_130
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_130
block_142:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_143
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_143
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_165
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_165
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_141
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_141
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_131
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_131
block_143:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_144
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_144
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_166
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_166
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_142
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_142
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_132
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_132
block_144:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_145
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_145
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_167
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_143
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_143
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_133
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_133
block_145:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_146
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_146
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_168
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_168
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_144
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_144
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_134
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_134
block_146:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_147
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_147
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_169
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_169
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_145
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_145
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_135
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_135
block_147:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_148
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_148
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_170
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_170
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_146
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_146
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_136
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_136
block_148:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_149
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_149
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_171
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_171
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_147
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_147
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_137
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_137
block_149:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_150
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_183
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_183
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_148
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_148
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_115
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_115
block_150:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_151
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_151
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_184
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_184
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_149
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_149
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_116
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_116
block_151:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_152
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_152
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_185
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_185
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_150
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_117
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_117
block_152:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_153
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_153
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_186
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_186
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_151
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_151
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_118
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_118
block_153:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_154
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_154
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_154
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_154
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_152
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_152
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_120
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_120
block_154:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_188
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_187
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_171
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_171
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_120
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_120
block_155:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_172
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_172
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_190
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_189
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_121
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_121
block_156:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_157
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_157
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_155
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_155
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_155
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_155
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_121
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_121
block_157:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_158
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_158
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_191
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_191
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_156
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_156
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_123
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_123
block_158:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_159
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_159
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_192
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_192
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_157
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_157
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_124
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_124
block_159:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_160
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_160
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_193
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_193
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_158
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_158
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_125
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_125
block_160:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_161
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_161
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_194
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_194
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_159
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_159
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_126
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_126
block_161:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_162
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_162
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_172
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_172
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_160
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_160
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_138
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_138
block_162:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_163
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_163
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_173
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_173
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_161
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_161
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_139
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_139
block_163:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_164
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_164
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_174
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_174
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_162
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_162
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_140
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_140
block_164:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_165
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_165
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_175
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_175
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_163
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_163
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_141
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_141
block_165:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_166
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_166
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_176
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_176
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_164
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_164
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_142
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_142
block_166:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_167
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_177
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_177
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_165
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_165
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_143
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_143
block_167:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_168
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_168
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_178
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_178
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_166
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_166
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_144
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_144
block_168:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_169
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_169
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_179
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_179
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_167
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_145
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_145
block_169:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_170
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_170
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_180
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_180
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_168
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_168
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_146
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_146
block_170:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_171
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_171
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_181
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_181
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_169
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_169
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_147
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_147
block_171:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_154
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_154
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_182
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_182
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_170
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_170
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_148
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_148
block_172:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_173
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_173
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_456
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_456
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_155
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_155
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_161
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_161
block_173:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_174
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_174
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_457
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_457
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_172
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_172
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_162
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_162
block_174:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_175
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_175
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_482
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_482
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_173
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_173
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_163
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_163
block_175:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_176
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_176
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_483
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_483
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_174
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_174
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_164
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_164
block_176:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_177
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_177
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_458
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_458
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_175
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_175
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_165
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_165
block_177:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_178
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_178
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_459
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_459
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_176
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_176
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_166
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_166
block_178:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_179
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_179
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_484
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_484
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_177
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_177
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_167
block_179:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_180
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_180
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_485
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_485
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_178
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_178
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_168
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_168
block_180:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_181
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_181
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_460
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_460
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_179
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_179
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_169
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_169
block_181:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_182
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_182
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_461
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_461
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_180
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_180
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_170
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_170
block_182:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_183
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_183
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_486
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_486
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_181
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_181
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_171
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_171
block_183:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_184
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_184
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_215
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_182
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_182
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_149
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_149
block_184:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_185
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_185
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_216
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_216
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_183
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_183
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_150
block_185:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_186
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_186
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_217
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_217
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_184
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_184
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_151
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_151
block_186:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_187
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_187
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_218
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_218
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_185
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_185
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_152
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_152
block_187:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_188
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_188
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_188
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_188
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_186
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_186
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_154
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_154
block_188:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_220
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_219
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_204
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_204
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_154
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_154
block_189:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_205
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_205
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_222
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_221
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_155
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_155
block_190:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_191
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_191
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_189
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_189
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_189
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_189
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_155
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_155
block_191:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_192
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_192
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_223
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_223
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_190
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_190
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_157
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_157
block_192:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_193
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_193
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_224
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_224
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_191
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_191
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_158
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_158
block_193:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_194
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_194
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_225
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_225
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_192
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_192
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_159
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_159
block_194:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_195
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_195
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_226
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_226
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_193
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_193
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_160
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_160
block_195:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_196
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_196
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_205
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_205
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_194
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_194
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_196:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_197
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_197
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_206
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_206
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_195
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_195
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_197:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_198
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_198
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_207
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_207
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_196
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_196
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_198:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_199
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_199
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_208
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_208
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_197
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_197
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_199:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_200
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_200
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_209
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_209
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_198
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_198
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_200:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_201
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_201
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_210
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_210
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_199
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_199
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_201:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_202
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_202
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_211
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_211
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_200
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_200
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_202:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_203
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_203
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_212
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_212
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_201
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_201
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_203:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_204
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_204
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_213
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_213
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_202
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_202
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_204:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_188
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_188
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_214
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_203
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_203
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_205:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_206
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_206
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_227
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_227
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_189
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_189
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_195
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_195
block_206:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_207
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_207
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_228
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_228
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_205
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_205
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_196
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_196
block_207:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_208
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_208
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_229
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_229
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_206
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_206
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_197
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_197
block_208:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_209
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_209
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_230
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_230
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_207
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_207
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_198
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_198
block_209:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_210
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_210
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_231
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_231
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_208
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_208
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_199
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_199
block_210:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_211
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_211
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_232
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_232
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_209
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_209
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_200
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_200
block_211:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_212
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_212
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_233
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_233
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_210
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_210
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_201
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_201
block_212:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_213
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_213
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_234
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_234
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_211
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_211
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_202
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_202
block_213:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_214
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_235
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_235
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_212
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_212
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_203
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_203
block_214:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_215
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_236
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_236
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_213
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_213
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_204
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_204
block_215:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_216
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_216
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_247
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_247
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_214
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_183
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_183
block_216:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_217
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_217
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_248
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_248
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_215
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_184
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_184
block_217:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_218
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_218
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_249
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_249
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_216
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_216
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_185
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_185
block_218:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_219
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_219
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_250
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_250
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_217
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_217
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_186
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_186
block_219:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_220
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_220
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_220
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_220
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_218
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_218
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_188
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_188
block_220:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_252
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_251
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_236
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_236
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_188
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_188
block_221:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_237
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_237
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_254
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_253
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_189
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_189
block_222:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_223
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_223
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_221
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_221
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_221
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_221
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_189
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_189
block_223:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_224
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_224
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_255
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_255
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_222
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_222
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_191
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_191
block_224:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_225
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_225
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_256
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_256
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_223
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_223
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_192
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_192
block_225:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_226
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_226
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_257
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_257
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_224
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_224
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_193
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_193
block_226:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_227
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_227
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_258
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_258
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_225
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_225
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_194
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_194
block_227:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_228
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_228
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_237
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_237
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_226
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_226
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_205
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_205
block_228:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_229
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_229
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_238
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_238
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_227
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_227
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_206
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_206
block_229:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_230
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_230
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_239
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_239
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_228
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_228
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_207
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_207
block_230:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_231
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_231
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_240
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_240
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_229
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_229
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_208
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_208
block_231:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_232
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_232
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_241
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_241
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_230
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_230
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_209
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_209
block_232:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_233
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_233
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_242
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_242
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_231
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_231
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_210
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_210
block_233:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_234
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_234
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_243
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_243
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_232
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_232
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_211
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_211
block_234:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_235
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_235
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_244
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_244
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_233
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_233
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_212
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_212
block_235:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_236
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_236
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_245
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_245
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_234
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_234
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_213
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_213
block_236:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_220
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_220
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_246
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_246
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_235
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_235
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_214
block_237:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_238
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_238
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_259
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_259
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_221
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_221
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_227
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_227
block_238:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_239
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_239
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_260
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_260
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_237
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_237
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_228
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_228
block_239:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_240
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_240
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_261
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_261
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_238
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_238
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_229
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_229
block_240:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_241
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_241
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_262
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_262
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_239
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_239
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_230
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_230
block_241:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_242
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_242
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_263
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_263
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_240
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_240
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_231
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_231
block_242:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_243
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_243
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_264
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_264
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_241
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_241
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_232
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_232
block_243:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_244
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_244
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_265
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_265
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_242
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_242
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_233
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_233
block_244:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_245
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_245
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_266
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_266
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_243
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_243
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_234
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_234
block_245:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_246
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_246
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_267
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_267
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_244
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_244
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_235
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_235
block_246:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_247
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_247
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_268
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_268
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_245
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_245
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_236
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_236
block_247:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_248
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_248
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_279
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_279
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_246
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_246
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_215
block_248:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_249
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_249
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_280
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_280
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_247
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_247
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_216
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_216
block_249:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_250
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_250
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_281
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_281
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_248
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_248
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_217
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_217
block_250:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_251
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_251
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_282
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_282
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_249
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_249
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_218
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_218
block_251:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_252
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_252
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_252
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_252
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_250
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_250
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_220
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_220
block_252:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_284
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_283
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_268
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_268
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_220
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_220
block_253:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_269
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_269
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_286
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_285
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_221
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_221
block_254:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_255
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_255
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_253
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_253
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_253
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_253
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_221
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_221
block_255:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_256
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_256
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_287
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_287
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_254
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_254
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_223
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_223
block_256:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_257
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_257
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_288
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_288
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_255
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_255
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_224
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_224
block_257:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_258
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_258
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_289
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_289
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_256
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_256
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_225
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_225
block_258:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_259
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_259
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_290
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_290
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_257
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_257
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_226
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_226
block_259:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_260
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_260
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_269
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_269
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_258
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_258
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_237
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_237
block_260:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_261
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_261
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_270
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_270
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_259
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_259
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_238
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_238
block_261:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_262
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_262
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_271
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_271
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_260
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_260
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_239
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_239
block_262:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_263
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_263
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_272
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_272
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_261
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_261
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_240
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_240
block_263:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_264
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_264
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_273
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_273
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_262
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_262
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_241
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_241
block_264:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_265
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_265
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_274
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_274
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_263
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_263
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_242
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_242
block_265:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_266
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_266
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_275
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_275
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_264
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_264
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_243
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_243
block_266:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_267
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_267
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_276
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_276
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_265
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_265
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_244
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_244
block_267:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_268
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_268
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_277
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_277
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_266
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_266
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_245
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_245
block_268:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_252
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_252
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_278
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_278
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_267
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_267
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_246
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_246
block_269:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_270
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_270
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_291
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_291
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_253
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_253
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_259
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_259
block_270:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_271
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_271
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_292
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_292
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_269
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_269
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_260
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_260
block_271:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_272
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_272
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_293
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_293
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_270
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_270
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_261
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_261
block_272:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_273
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_273
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_294
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_294
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_271
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_271
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_262
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_262
block_273:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_274
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_274
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_295
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_295
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_272
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_272
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_263
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_263
block_274:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_275
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_275
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_296
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_296
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_273
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_273
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_264
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_264
block_275:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_276
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_276
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_297
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_297
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_274
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_274
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_265
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_265
block_276:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_277
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_277
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_298
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_298
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_275
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_275
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_266
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_266
block_277:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_278
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_278
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_299
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_299
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_276
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_276
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_267
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_267
block_278:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_279
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_279
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_300
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_300
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_277
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_277
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_268
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_268
block_279:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_280
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_280
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_311
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_311
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_278
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_278
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_247
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_247
block_280:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_281
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_281
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_312
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_312
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_279
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_279
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_248
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_248
block_281:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_282
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_282
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_313
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_313
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_280
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_280
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_249
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_249
block_282:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_283
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_283
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_314
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_314
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_281
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_281
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_250
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_250
block_283:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_284
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_284
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_284
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_284
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_282
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_282
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_252
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_252
block_284:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_316
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_315
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_300
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_300
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_252
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_252
block_285:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_301
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_301
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_318
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_317
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_253
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_253
block_286:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_287
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_287
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_285
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_285
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_285
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_285
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_253
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_253
block_287:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_288
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_288
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_319
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_319
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_286
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_286
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_255
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_255
block_288:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_289
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_289
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_320
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_320
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_287
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_287
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_256
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_256
block_289:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_290
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_290
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_321
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_321
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_288
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_288
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_257
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_257
block_290:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_291
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_291
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_322
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_322
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_289
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_289
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_258
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_258
block_291:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_292
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_292
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_301
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_301
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_290
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_290
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_269
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_269
block_292:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_293
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_293
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_302
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_302
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_291
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_291
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_270
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_270
block_293:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_294
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_294
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_303
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_303
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_292
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_292
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_271
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_271
block_294:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_295
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_295
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_304
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_304
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_293
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_293
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_272
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_272
block_295:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_296
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_296
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_305
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_305
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_294
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_294
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_273
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_273
block_296:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_297
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_297
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_306
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_306
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_295
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_295
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_274
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_274
block_297:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_298
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_298
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_307
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_307
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_296
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_296
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_275
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_275
block_298:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_299
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_299
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_308
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_308
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_297
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_297
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_276
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_276
block_299:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_300
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_300
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_309
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_309
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_298
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_298
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_277
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_277
block_300:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_284
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_284
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_310
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_310
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_299
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_299
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_278
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_278
block_301:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_302
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_302
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_323
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_323
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_285
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_285
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_291
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_291
block_302:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_303
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_303
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_324
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_324
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_301
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_301
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_292
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_292
block_303:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_304
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_304
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_325
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_325
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_302
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_302
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_293
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_293
block_304:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_305
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_305
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_326
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_326
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_303
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_303
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_294
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_294
block_305:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_306
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_306
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_327
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_327
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_304
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_304
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_295
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_295
block_306:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_307
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_307
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_328
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_328
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_305
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_305
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_296
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_296
block_307:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_308
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_308
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_329
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_329
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_306
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_306
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_297
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_297
block_308:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_309
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_309
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_330
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_330
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_307
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_307
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_298
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_298
block_309:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_310
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_310
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_331
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_331
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_308
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_308
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_299
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_299
block_310:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_311
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_311
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_332
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_332
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_309
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_309
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_300
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_300
block_311:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_312
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_312
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_343
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_343
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_310
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_310
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_279
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_279
block_312:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_313
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_313
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_344
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_344
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_311
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_311
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_280
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_280
block_313:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_314
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_314
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_345
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_345
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_312
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_312
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_281
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_281
block_314:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_315
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_315
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_346
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_346
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_313
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_313
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_282
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_282
block_315:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_316
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_316
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_316
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_316
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_314
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_314
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_284
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_284
block_316:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_348
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_347
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_332
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_332
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_284
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_284
block_317:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_333
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_333
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_350
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_349
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_285
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_285
block_318:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_319
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_319
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_317
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_317
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_317
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_317
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_285
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_285
block_319:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_320
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_320
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_351
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_351
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_318
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_318
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_287
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_287
block_320:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_321
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_321
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_352
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_352
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_319
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_319
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_288
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_288
block_321:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_322
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_322
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_353
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_353
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_320
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_320
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_289
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_289
block_322:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_323
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_323
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_354
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_354
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_321
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_321
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_290
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_290
block_323:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_324
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_324
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_333
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_333
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_322
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_322
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_301
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_301
block_324:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_325
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_325
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_334
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_334
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_323
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_323
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_302
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_302
block_325:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_326
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_326
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_335
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_335
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_324
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_324
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_303
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_303
block_326:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_327
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_327
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_336
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_336
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_325
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_325
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_304
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_304
block_327:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_328
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_328
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_337
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_337
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_326
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_326
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_305
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_305
block_328:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_329
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_329
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_338
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_338
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_327
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_327
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_306
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_306
block_329:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_330
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_330
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_339
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_339
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_328
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_328
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_307
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_307
block_330:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_331
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_331
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_340
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_340
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_329
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_329
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_308
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_308
block_331:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_332
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_332
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_341
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_341
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_330
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_330
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_309
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_309
block_332:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_316
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_316
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_342
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_342
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_331
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_331
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_310
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_310
block_333:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_334
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_334
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_487
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_487
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_317
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_317
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_323
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_323
block_334:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_335
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_335
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_462
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_462
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_333
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_333
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_324
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_324
block_335:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_336
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_336
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_463
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_463
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_334
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_334
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_325
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_325
block_336:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_337
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_337
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_488
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_488
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_335
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_335
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_326
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_326
block_337:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_338
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_338
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_489
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_489
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_336
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_336
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_327
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_327
block_338:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_339
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_339
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_464
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_464
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_337
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_337
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_328
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_328
block_339:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_340
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_340
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_465
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_465
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_338
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_338
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_329
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_329
block_340:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_341
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_341
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_490
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_490
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_339
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_339
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_330
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_330
block_341:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_342
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_342
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_491
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_491
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_340
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_340
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_331
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_331
block_342:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_343
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_343
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_466
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_466
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_341
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_341
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_332
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_332
block_343:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_344
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_344
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_369
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_369
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_342
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_342
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_311
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_311
block_344:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_345
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_345
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_370
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_370
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_343
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_343
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_312
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_312
block_345:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_346
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_346
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_371
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_371
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_344
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_344
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_313
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_313
block_346:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_347
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_347
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_372
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_372
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_345
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_345
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_314
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_314
block_347:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_348
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_348
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_348
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_348
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_346
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_346
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_316
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_316
block_348:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_374
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_373
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_361
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_361
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_316
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_316
block_349:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_362
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_362
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_376
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_375
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_317
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_317
block_350:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_351
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_351
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_349
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_349
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_349
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_349
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_317
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_317
block_351:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_352
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_352
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_377
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_377
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_350
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_350
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_319
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_319
block_352:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_353
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_353
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_378
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_378
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_351
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_351
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_320
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_320
block_353:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_354
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_354
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_379
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_379
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_352
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_352
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_321
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_321
block_354:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_355
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_355
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_380
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_380
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_353
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_353
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_322
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_322
block_355:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_356
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_356
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_362
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_362
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_354
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_354
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_356:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_357
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_357
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_363
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_363
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_355
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_355
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_357:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_358
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_358
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_364
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_364
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_356
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_356
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_358:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_359
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_359
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_365
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_365
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_357
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_357
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_359:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_360
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_360
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_366
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_366
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_358
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_358
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_360:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_361
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_361
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_367
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_367
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_359
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_359
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_361:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_348
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_348
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_368
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_368
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_360
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_360
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_362:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_363
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_363
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_381
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_381
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_349
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_349
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_355
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_355
block_363:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_364
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_364
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_382
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_382
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_362
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_362
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_356
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_356
block_364:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_365
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_365
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_383
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_383
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_363
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_363
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_357
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_357
block_365:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_366
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_366
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_384
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_384
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_364
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_364
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_358
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_358
block_366:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_367
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_367
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_385
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_385
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_365
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_365
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_359
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_359
block_367:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_368
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_368
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_386
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_386
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_366
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_366
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_360
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_360
block_368:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_369
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_369
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_387
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_387
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_367
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_367
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_361
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_361
block_369:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_370
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_370
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_395
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_395
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_368
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_368
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_343
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_343
block_370:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_371
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_371
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_396
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_396
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_369
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_369
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_344
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_344
block_371:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_372
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_372
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_397
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_397
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_370
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_370
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_345
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_345
block_372:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_373
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_373
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_398
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_398
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_371
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_371
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_346
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_346
block_373:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_374
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_374
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_374
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_374
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_372
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_372
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_348
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_348
block_374:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_400
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_399
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_387
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_387
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_348
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_348
block_375:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_388
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_388
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_402
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_401
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_349
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_349
block_376:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_377
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_377
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_375
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_375
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_375
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_375
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_349
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_349
block_377:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_378
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_378
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_403
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_403
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_376
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_376
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_351
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_351
block_378:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_379
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_379
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_404
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_404
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_377
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_377
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_352
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_352
block_379:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_380
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_380
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_405
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_405
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_378
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_378
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_353
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_353
block_380:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_381
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_381
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_406
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_406
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_379
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_379
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_354
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_354
block_381:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_382
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_382
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_388
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_388
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_380
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_380
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_362
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_362
block_382:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_383
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_383
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_389
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_389
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_381
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_381
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_363
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_363
block_383:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_384
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_384
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_390
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_390
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_382
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_382
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_364
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_364
block_384:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_385
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_385
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_391
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_391
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_383
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_383
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_365
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_365
block_385:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_386
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_386
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_392
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_392
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_384
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_384
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_366
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_366
block_386:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_387
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_387
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_393
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_393
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_385
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_385
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_367
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_367
block_387:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_374
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_374
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_394
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_394
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_386
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_386
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_368
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_368
block_388:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_389
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_389
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_407
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_407
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_375
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_375
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_381
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_381
block_389:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_390
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_390
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_408
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_408
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_388
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_388
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_382
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_382
block_390:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_391
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_391
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_409
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_409
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_389
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_389
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_383
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_383
block_391:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_392
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_392
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_410
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_410
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_390
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_390
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_384
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_384
block_392:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_393
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_393
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_411
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_411
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_391
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_391
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_385
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_385
block_393:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_394
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_394
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_412
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_412
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_392
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_392
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_386
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_386
block_394:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_395
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_395
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_413
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_413
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_393
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_393
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_387
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_387
block_395:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_396
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_396
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_421
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_421
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_394
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_394
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_369
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_369
block_396:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_397
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_397
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_422
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_422
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_395
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_395
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_370
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_370
block_397:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_398
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_398
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_423
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_423
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_396
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_396
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_371
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_371
block_398:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_399
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_399
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_424
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_424
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_397
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_397
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_372
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_372
block_399:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_400
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_400
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_400
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_400
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_398
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_398
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_374
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_374
block_400:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_426
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_425
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_413
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_413
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_374
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_374
block_401:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_414
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_414
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_428
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_427
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_375
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_375
block_402:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_403
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_403
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_401
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_401
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_401
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_401
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_375
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_375
block_403:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_404
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_404
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_429
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_429
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_402
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_402
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_377
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_377
block_404:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_405
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_405
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_430
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_430
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_403
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_403
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_378
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_378
block_405:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_406
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_406
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_431
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_431
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_404
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_404
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_379
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_379
block_406:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_407
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_407
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_432
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_432
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_405
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_405
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_380
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_380
block_407:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_408
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_408
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_414
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_414
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_406
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_406
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_388
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_388
block_408:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_409
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_409
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_415
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_415
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_407
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_407
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_389
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_389
block_409:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_410
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_410
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_416
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_416
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_408
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_408
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_390
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_390
block_410:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_411
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_411
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_417
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_417
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_409
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_409
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_391
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_391
block_411:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_412
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_412
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_418
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_418
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_410
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_410
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_392
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_392
block_412:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_413
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_413
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_419
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_419
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_411
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_411
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_393
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_393
block_413:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_400
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_400
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_420
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_420
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_412
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_412
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_394
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_394
block_414:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_415
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_415
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_433
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_433
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_401
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_401
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_407
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_407
block_415:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_416
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_416
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_434
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_434
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_414
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_414
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_408
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_408
block_416:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_417
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_417
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_435
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_435
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_415
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_415
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_409
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_409
block_417:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_418
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_418
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_436
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_436
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_416
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_416
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_410
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_410
block_418:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_419
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_419
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_437
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_437
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_417
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_417
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_411
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_411
block_419:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_420
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_420
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_438
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_438
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_418
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_418
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_412
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_412
block_420:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_421
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_421
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_439
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_439
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_419
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_419
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_413
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_413
block_421:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_422
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_422
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_442
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_442
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_420
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_420
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_395
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_395
block_422:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_423
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_423
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_443
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_443
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_421
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_421
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_396
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_396
block_423:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_424
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_424
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_444
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_444
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_422
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_422
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_397
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_397
block_424:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_425
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_425
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_445
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_445
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_423
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_423
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_398
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_398
block_425:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_426
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_426
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_426
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_426
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_424
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_424
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_400
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_400
block_426:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_447
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_446
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_439
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_439
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_400
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_400
block_427:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_440
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_440
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_449
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_448
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_401
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_401
block_428:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_429
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_429
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_427
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_427
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_427
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_427
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_401
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_401
block_429:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_430
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_430
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_450
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_450
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_428
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_428
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_403
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_403
block_430:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_431
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_431
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_451
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_451
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_429
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_429
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_404
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_404
block_431:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_432
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_432
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_452
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_452
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_430
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_430
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_405
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_405
block_432:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_433
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_433
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_453
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_453
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_431
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_431
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_406
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_406
block_433:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_434
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_434
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_467
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_467
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_432
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_432
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_414
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_414
block_434:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_435
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_435
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_492
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_492
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_433
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_433
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_415
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_415
block_435:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_436
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_436
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_493
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_493
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_434
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_434
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_416
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_416
block_436:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_437
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_437
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_468
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_468
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_435
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_435
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_417
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_417
block_437:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_438
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_438
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_469
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_469
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_436
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_436
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_418
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_418
block_438:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_439
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_439
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_494
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_494
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_437
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_437
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_419
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_419
block_439:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_426
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_426
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_495
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_495
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_438
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_438
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_420
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_420
block_440:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_441
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_441
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_454
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_454
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_427
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_427
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_441:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_442
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_442
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_455
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_455
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_440
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_440
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_442:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_443
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_443
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_470
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_470
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_441
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_441
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_421
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_421
block_443:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_444
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_444
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_471
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_471
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_442
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_442
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_422
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_422
block_444:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_445
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_445
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_472
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_472
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_443
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_443
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_423
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_423
block_445:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_446
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_446
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_473
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_473
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_444
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_444
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_424
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_424
block_446:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_447
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_447
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_447
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_447
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_445
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_445
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_426
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_426
block_447:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_475
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_474
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_455
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_455
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_426
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_426
block_448:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_456
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_456
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_477
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_427
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_427
block_449:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_450
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_450
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_448
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_448
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_448
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_448
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_427
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_427
block_450:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_451
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_451
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_478
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_478
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_449
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_449
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_429
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_429
block_451:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_452
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_452
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_479
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_479
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_450
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_450
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_430
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_430
block_452:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_453
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_453
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_480
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_480
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_451
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_451
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_431
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_431
block_453:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_454
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_454
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_481
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_481
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_452
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_452
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_432
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_432
block_454:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_455
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_455
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_453
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_453
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_440
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_440
block_455:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_447
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_447
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_454
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_454
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_441
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_441
block_456:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_457
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_457
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_448
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_448
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_172
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_172
block_457:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_458
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_458
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_456
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_456
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_173
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_173
block_458:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_459
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_459
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_457
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_457
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_176
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_176
block_459:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_460
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_460
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_458
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_458
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_177
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_177
block_460:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_461
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_461
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_459
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_459
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_180
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_180
block_461:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_462
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_462
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_460
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_460
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_181
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_181
block_462:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_463
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_463
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_461
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_461
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_334
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_334
block_463:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_464
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_464
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_462
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_462
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_335
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_335
block_464:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_465
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_465
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_463
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_463
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_338
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_338
block_465:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_466
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_466
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_464
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_464
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_339
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_339
block_466:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_467
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_467
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_465
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_465
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_342
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_342
block_467:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_468
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_468
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_466
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_466
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_433
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_433
block_468:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_469
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_469
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_467
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_467
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_436
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_436
block_469:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_470
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_470
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_468
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_468
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_437
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_437
block_470:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_471
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_471
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_469
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_469
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_442
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_442
block_471:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_472
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_472
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_470
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_470
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_443
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_443
block_472:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_473
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_473
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_471
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_471
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_444
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_444
block_473:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_474
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_474
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_496
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_496
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_472
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_472
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_445
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_445
block_474:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_475
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_475
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_475
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_475
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_473
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_473
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_447
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_447
block_475:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_496
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_496
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_495
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_495
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_447
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_447
block_476:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_496
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_496
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_448
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_448
block_477:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_478
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_478
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_448
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_448
block_478:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_479
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_479
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_477
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_477
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_450
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_450
block_479:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_480
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_480
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_478
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_478
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_451
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_451
block_480:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_481
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_481
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_479
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_479
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_452
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_452
block_481:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_482
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_482
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_480
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_480
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_453
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_453
block_482:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_483
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_483
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_481
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_481
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_174
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_174
block_483:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_484
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_484
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_482
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_482
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_175
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_175
block_484:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_485
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_485
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_483
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_483
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_178
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_178
block_485:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_486
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_486
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_484
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_484
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_179
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_179
block_486:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_487
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_487
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_485
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_485
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_182
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_182
block_487:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_488
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_488
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_486
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_486
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_333
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_333
block_488:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_489
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_489
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_487
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_487
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_336
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_336
block_489:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_490
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_490
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_488
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_488
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_337
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_337
block_490:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_491
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_491
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_489
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_489
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_340
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_340
block_491:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_492
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_492
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_490
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_490
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_341
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_341
block_492:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_493
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_493
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_491
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_491
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_434
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_434
block_493:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_494
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_494
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_492
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_492
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_435
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_435
block_494:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_495
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_495
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_493
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_493
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_438
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_438
block_495:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_475
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_475
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_494
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_494
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_439
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_439
block_496:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_496
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_496
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
