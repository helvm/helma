; --- PIET ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 72
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 72
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_1
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_16
block_1:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
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
    push 101
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_0
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_0
block_3:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
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
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 108
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 108
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_2
block_8:
block_13:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_15
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
block_15:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 33
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_13
block_16:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 100
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_15
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_2
block_18:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_35
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_35
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 111
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 111
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_4
block_20:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_4
block_21:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_23
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_20
block_22:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_15
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_15
block_23:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_25
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
block_25:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_23
block_26:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_27
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_27
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_15
block_27:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_16
block_28:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 114
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_26
block_31:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_34
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 119
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 119
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_16
block_33:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_31
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_38
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_28
block_34:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_36
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_31
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_18
block_35:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_36
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_34
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_18
block_36:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_34
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_35
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_35
block_37:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 32
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 32
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_34
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 32
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_36
block_38:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
