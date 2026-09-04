; --- PIET ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_1
block_1:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
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
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_1
block_3:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
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
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
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
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_7
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
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
    in_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_6
block_8:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_7
block_9:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_10
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_8
block_10:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_9
block_11:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_10
block_12:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
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
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_12
block_14:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_13
block_15:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
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
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_15
block_17:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_16
block_18:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_17
block_19:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
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
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_19
block_21:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_22
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_20
block_22:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 43
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_40
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_40
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_40
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_21
block_23:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
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
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_25
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_23
block_25:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 61
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_26
block_26:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_27
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_27
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_25
block_27:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_26
block_28:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_29
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_27
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_27
block_29:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_30
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_28
block_30:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_46
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_46
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_29
block_36:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_38
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_38
block_37:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_40
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_40
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_44
block_38:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_36
block_39:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_43
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_43
block_40:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 25
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_45
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_22
block_41:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 13
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_42
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 13
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_42
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_36
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_36
block_42:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_43
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_43
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_41
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_41
block_43:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_39
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 9
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_42
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 9
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_42
block_44:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_39
block_45:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_40
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_40
block_46:
