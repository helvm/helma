; --- PIET ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_1
block_1:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_num
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
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_1
block_3:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
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
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_3
block_5:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_6
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_4
block_6:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_5
block_8:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_9
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_6
block_9:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_10
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_8
block_10:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_9
block_12:
block_17:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
block_19:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_20
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_6
block_20:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_24
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_19
block_24:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_20
block_26:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_20
block_28:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_29
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_26
block_29:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_30
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_30
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
block_30:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_31
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_29
block_31:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_32
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_32
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_30
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_30
block_32:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_34
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_31
block_33:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_35
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_35
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_2
block_34:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_36
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_32
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_32
block_35:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_33
block_36:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_39
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_34
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_34
block_37:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_39
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_35
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_35
block_39:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_36
