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
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_1
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
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
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_1
block_4:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
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
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_6
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_4
block_6:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_7
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_7
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_5
block_7:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_6
block_8:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_9
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_7
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_7
block_9:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_10
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_1
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_8
block_10:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_9
block_11:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_70
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_70
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
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_85
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_85
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
block_13:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
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
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_13
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_13
block_15:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_65
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
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_90
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_90
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_15
block_19:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 14
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 14
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_29
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_10
block_20:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_23
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
block_23:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_24
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_20
block_24:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_23
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
block_26:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_20
block_28:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_28
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_38
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_38
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_26
block_29:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_30
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_30
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_19
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_19
block_30:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_29
block_33:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_30
block_35:
block_37:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_41
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 10
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_41
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_33
block_38:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_35
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_35
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_39
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
block_39:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_42
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_42
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_38
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_38
block_41:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_46
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_37
block_42:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_39
block_44:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 25
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_42
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 25
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_42
block_46:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_47
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_47
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_41
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_41
block_47:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_48
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_48
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_46
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_46
block_48:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_50
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_50
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_47
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_47
block_50:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 15
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_55
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_48
block_53:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_56
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_56
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_44
block_55:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_58
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_50
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_50
block_56:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_59
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_59
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_53
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_53
block_58:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_60
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_60
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_55
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_55
block_59:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_56
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_56
block_60:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_62
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_62
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_58
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_58
block_61:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_61
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_66
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_59
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_59
block_62:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_64
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_60
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_60
block_64:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_62
block_65:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_70
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_64
block_66:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_73
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_73
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_61
block_69:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_70
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_69
block_70:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_85
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_85
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_75
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_75
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_69
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_65
block_73:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_66
block_75:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_78
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_78
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_70
block_77:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 10
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_73
block_78:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_79
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_79
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_75
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_75
block_79:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_81
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_69
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_78
block_81:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_84
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_84
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_79
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_79
block_84:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_86
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_84
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_84
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_81
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_81
block_85:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_88
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_88
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_12
block_86:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_88
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_88
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_110
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_110
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_84
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_84
block_88:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_89
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_86
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_85
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_85
block_89:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_90
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_90
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_88
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_88
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_90:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_91
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_91
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_89
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_89
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_16
block_91:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_92
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_92
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_90
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_90
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_92:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 17
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_93
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 17
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_93
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_91
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_93:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_94
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_94
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_92
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_94:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_95
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_93
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_93
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_95:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_96
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_96
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_94
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_94
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_96:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_97
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_97
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_95
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_95
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_97:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_98
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_98
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_96
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_96
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_98:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 15
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_99
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_97
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_99:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_100
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_98
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_98
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_100:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_101
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_101
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_99
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_99
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_101:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_102
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_102
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_100
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_100
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_102:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_103
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_103
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_101
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_101
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_103:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_102
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_104:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_105
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_105
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_103
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_103
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_105:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_106
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_106
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_104
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_106:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_107
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_107
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_105
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_105
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_107:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_108
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_108
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_106
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_106
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_108:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_107
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_107
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
block_110:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_86
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_86
