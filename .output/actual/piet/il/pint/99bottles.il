; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_1
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_19
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_2
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_0
block_2:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_1
block_3:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_4
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_4
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_2
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_3
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_6
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_6
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_4
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_7
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_7
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_5
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_6
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_9
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_9
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_7
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_9
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_10
block_13:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_11
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_13
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_14
block_17:
    switch_dpcc
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_19
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_19
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_0
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_0
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_1
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_1
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_3
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_23
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_4
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_24
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_5
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_25
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_23
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_6
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_6
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_7
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_7
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_27
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_82
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_25
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_8
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_8
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_10
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_229
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_229
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_11
block_29:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_30:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_31
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_32
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_32:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_83
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_34
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_35
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_35:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_36
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_36:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_37
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_38
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_38
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_39
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_98
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_41
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_42
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_13
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_45
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_85
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_14
block_45:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_46
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_46
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_46:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_47
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_47
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_47:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_48
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_48:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_49
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_47
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_49:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_50
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_50:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_51
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_51:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_52
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_52:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_53
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_53
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_53:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_54
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_54
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_54:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_55
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_55:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_56
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_56:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_57
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_87
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_87
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_57:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_58
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_58
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_58:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_59
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_107
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_59:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_60
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_60:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_61:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_62
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_62:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_63
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_63
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_63:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_64
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_65
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_65:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_66
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_66:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_67
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_67
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_67:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_68
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_68
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_68:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_69
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_69
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_89
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_67
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_67
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_69:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_70
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_116
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_70:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_71
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_71:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_72
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_72:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_73
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_73:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_74:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_75
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_73
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_75:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_76
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_119
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_119
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_76:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_120
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_77:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_78
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_78
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_78:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_79:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_80
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_80
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_80:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_91
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_81:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_82
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_92
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_92
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_18
block_82:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_83
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_94
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_94
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_26
block_83:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_95
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_95
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_82
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_32
block_84:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_85
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_83
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_38
block_85:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_86
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_44
block_86:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_87
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_87
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_85
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_50
block_87:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_56
block_88:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_89
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_89
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_87
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_62
block_89:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_68
block_90:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_91
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_74
block_91:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_80
block_92:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_81
block_93:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_92
block_94:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_95
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_95
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_124
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_124
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_82
block_95:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_96
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_124
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_83
block_96:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_97
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_182
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_84
block_97:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_98
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_125
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_125
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_98:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_99
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_97
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_39
block_99:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_100
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_162
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_184
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_184
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_85
block_100:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_101
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_162
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_162
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_86
block_101:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_102
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_145
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_163
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_163
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_87
block_102:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_103
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_103
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_103:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_104
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_104:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_105
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_105
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_129
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_105:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_106
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_130
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_130
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_106:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_107
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_107
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_131
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_57
block_107:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_132
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_132
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_58
block_108:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_109
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_150
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_168
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_168
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_88
block_109:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_110
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_133
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_110:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_111
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_170
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_194
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_169
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_111:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_112
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_112
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_134
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_112:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_113
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_135
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_113:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_114
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_136
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_112
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_63
block_114:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_137
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_113
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_64
block_115:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_116
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_155
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_174
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_154
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_89
block_116:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_117
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_69
block_117:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_118
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_70
block_118:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_119
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_156
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_176
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_155
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_117
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_90
block_119:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_120
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_226
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_139
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_75
block_120:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_119
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_76
block_121:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_122
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_157
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_178
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_178
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_156
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_91
block_122:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_123
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_123
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_140
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_123:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_124:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_95
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_95
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_141
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_94
block_125:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_126
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_143
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_97
block_126:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_99
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_144
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_125
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_98
block_127:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_128
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_145
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_145
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_102
block_128:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_129
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_165
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_188
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_164
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_103
block_129:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_130
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_130
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_146
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_146
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_104
block_130:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_131
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_131
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_147
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_147
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_105
block_131:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_132
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_132
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_148
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_148
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_130
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_106
block_132:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_107
block_133:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_110
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_150
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_109
block_134:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_135
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_151
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_111
block_135:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_136
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_152
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_152
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_112
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_112
block_136:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_137
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_135
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_135
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_113
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_113
block_137:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_154
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_136
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_114
block_138:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_118
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_155
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_155
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_116
block_139:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_156
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_156
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_119
block_140:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_157
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_157
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_122
block_141:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_142
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_142
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_124
block_142:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_96
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_159
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_141
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_95
block_143:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_144
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_144
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_125
block_144:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_99
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_161
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_143
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_126
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_126
block_145:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_128
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_164
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_164
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_127
block_146:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_147
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_147
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_165
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_165
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_129
block_147:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_148
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_148
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_166
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_166
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_146
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_146
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_130
block_148:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_149
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_167
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_147
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_131
block_149:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_192
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_216
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_216
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_191
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_132
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_132
block_150:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_110
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_169
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_169
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_133
block_151:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_152
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_152
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_170
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_170
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_134
block_152:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_153
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_196
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_151
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_135
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_135
block_153:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_154
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_172
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_172
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_152
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_152
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_136
block_154:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_173
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_173
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_153
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_137
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_137
block_155:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_118
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_175
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_175
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_138
block_156:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_177
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_177
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_139
block_157:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_179
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_140
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_140
block_158:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_159
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_159
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_180
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_141
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_27
block_159:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_96
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_181
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_181
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_142
block_160:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_161
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_161
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_183
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_183
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_143
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_143
block_161:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_99
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_184
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_208
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_208
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_183
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_160
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_144
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_144
block_162:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_163
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_163
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_100
block_163:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_164
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_164
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_186
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_186
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_162
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_162
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_101
block_164:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_128
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_212
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_212
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_163
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_163
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_145
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_145
block_165:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_166
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_166
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_189
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_189
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_146
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_146
block_166:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_167
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_190
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_165
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_147
block_167:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_149
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_191
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_191
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_166
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_166
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_148
block_168:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_169
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_169
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_192
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_192
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_108
block_169:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_110
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_193
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_193
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_168
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_168
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_150
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_150
block_170:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_172
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_172
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_195
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_151
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_151
block_172:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_173
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_173
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_170
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_153
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_153
block_173:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_174
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_174
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_198
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_172
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_172
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_154
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_154
block_174:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_175
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_175
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_199
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_199
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_173
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_173
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_115
block_175:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_176
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_341
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_341
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_155
block_176:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_177
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_177
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_200
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_200
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_175
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_118
block_177:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_178
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_178
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_226
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_226
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_176
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_156
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_156
block_178:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_179
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_201
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_201
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_177
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_177
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_121
block_179:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_488
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_488
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_178
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_157
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_157
block_180:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_181
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_181
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_203
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_203
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_158
block_181:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_182
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_182
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_204
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_204
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_180
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_159
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_159
block_182:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_183
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_183
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_206
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_181
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_96
block_183:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_161
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_161
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_207
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_182
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_160
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_160
block_184:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_185
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_209
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_209
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_161
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_99
block_185:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_186
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_186
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_184
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_162
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_162
block_186:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_188
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_211
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_211
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_185
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_163
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_163
block_188:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_189
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_189
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_213
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_213
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_186
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_128
block_189:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_190
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_214
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_188
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_165
block_190:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_191
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_303
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    jump block_303
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_283
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_166
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_166
block_191:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_149
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_215
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_190
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_190
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_167
block_192:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_193
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_193
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_217
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_217
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_168
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_168
block_193:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_194
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_194
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_218
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_218
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_192
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_192
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_169
block_194:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_195
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_195
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_219
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_219
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_193
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_193
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_110
block_195:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_196
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_196
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_220
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_170
block_196:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_197
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_221
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_221
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_152
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_152
block_197:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_198
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_198
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_222
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_222
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_172
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_172
block_198:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_199
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_199
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_223
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_223
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_173
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_173
block_199:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_200
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_200
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_224
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_224
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_174
block_200:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_201
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_201
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_225
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_225
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_199
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_176
block_201:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_227
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_227
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_200
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_200
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_178
block_202:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_203
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_203
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_228
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_228
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_158
block_203:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_204
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_204
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_229
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_229
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_180
block_204:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_205
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_203
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_181
block_205:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_206
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_230
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_294
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_253
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_229
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_95
block_206:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_207
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_207
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_230
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_230
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_182
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_182
block_207:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_208
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_208
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_231
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_231
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_183
block_208:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_209
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_209
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_232
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_232
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_207
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_161
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_161
block_209:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_210
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_233
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_233
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_208
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_208
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_184
block_210:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_211
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_211
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_234
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_234
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_209
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_185
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_185
block_211:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_212
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_212
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_235
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_235
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_186
block_212:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_213
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_213
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_236
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_236
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_211
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_164
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_164
block_213:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_214
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_237
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_237
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_212
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_188
block_214:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_190
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_238
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_238
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_213
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_189
block_215:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_216
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_216
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_190
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_190
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_191
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_191
block_216:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_217
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_217
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_240
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_240
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_149
block_217:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_218
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_218
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_241
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_241
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_216
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_216
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_192
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_192
block_218:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_219
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_242
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_265
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_265
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_241
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_217
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_193
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_193
block_219:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_220
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_242
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_242
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_218
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_218
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_194
block_220:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_221
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_267
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_307
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_307
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_287
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_219
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_195
block_221:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_222
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_222
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_243
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_243
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_220
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_220
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_196
block_222:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_223
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_223
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_244
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_244
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_221
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_197
block_223:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_224
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_224
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_245
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_245
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_222
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_222
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_198
block_224:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_225
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_225
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_246
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_246
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_223
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_223
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_199
block_225:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_226
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_226
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_247
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_247
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_224
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_224
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_200
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_200
block_226:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_248
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_248
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_560
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_661
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_247
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_225
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_177
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_177
block_227:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_248
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_248
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_226
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_201
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_201
block_228:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_229
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_229
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_249
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_249
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_202
block_229:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_205
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_252
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_251
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_228
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_228
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_28
block_230:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_231
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_231
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_254
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_254
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_206
block_231:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_232
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_232
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_255
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_255
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_230
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_207
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_207
block_232:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_233
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_233
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_256
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_256
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_231
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_231
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_208
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_208
block_233:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_234
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_234
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_257
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_257
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_232
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_209
block_234:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_235
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_235
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_258
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_258
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_233
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_233
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_210
block_235:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_236
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_236
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_259
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_234
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_234
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_211
block_236:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_237
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_237
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_260
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_260
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_235
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_212
block_237:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_238
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_238
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_261
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_261
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_236
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_213
block_238:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_190
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_262
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_262
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_237
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_237
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_214
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_214
block_239:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_240
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_354
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_371
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_371
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_353
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_190
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 7
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 7
    jump block_215
block_240:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_241
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_241
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_263
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_216
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_216
block_241:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_218
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_218
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_264
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_240
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_240
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_217
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_217
block_242:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_220
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_266
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_218
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_218
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_219
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_219
block_243:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_244
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_244
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_267
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_267
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_220
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_220
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_221
block_244:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_245
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_245
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_268
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_268
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_243
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_243
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_222
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_222
block_245:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_246
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_246
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_269
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_269
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_244
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_244
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_223
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_223
block_246:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_247
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_247
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_270
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_270
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_245
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_245
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_224
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_224
block_247:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_226
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_226
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_271
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_271
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_246
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_246
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_225
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_225
block_248:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_272
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_272
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_226
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_227
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_227
block_249:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_250
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_250
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_273
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_273
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_228
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_228
block_250:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_251
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_312
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_326
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    jump block_326
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_158
block_251:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_274
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_293
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_312
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    jump block_312
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_250
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_229
block_252:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_253
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_253
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_251
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_251
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_251
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_251
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_229
block_253:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_254
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_254
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_274
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_274
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_252
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_252
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_205
block_254:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_255
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_255
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_275
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_275
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_253
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_230
block_255:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_256
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_256
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_276
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_276
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_254
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_254
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_231
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_231
block_256:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_257
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_257
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_277
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_277
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_255
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_255
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_232
block_257:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_258
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_258
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_278
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_278
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_256
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_256
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_233
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_233
block_258:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_259
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_259
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_279
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_279
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_257
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_257
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_234
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_234
block_259:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_260
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_260
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_280
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_280
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_258
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_258
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_235
block_260:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_261
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_261
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_281
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_281
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_259
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_236
block_261:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_262
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_262
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_282
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_282
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_260
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_260
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_237
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_237
block_262:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_190
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_283
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_283
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_261
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_261
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_238
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_238
block_263:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_264
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_264
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_284
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_284
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_240
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_240
block_264:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_265
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_265
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_285
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_285
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_263
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_241
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_241
block_265:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_266
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_266
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_286
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_286
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_264
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_218
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_218
block_266:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_220
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_287
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_287
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_265
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_265
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_242
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_242
block_267:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_268
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_308
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_321
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_321
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_307
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_220
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_243
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_243
block_268:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_269
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_269
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_308
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_308
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_267
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_267
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_244
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_244
block_269:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_270
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_323
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_339
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_339
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_322
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_268
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_245
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_245
block_270:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_271
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_271
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_289
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_289
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_269
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_269
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_246
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_246
block_271:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_272
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_272
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_290
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_290
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_270
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_270
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_247
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_247
block_272:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_291
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_291
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_271
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_271
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_248
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_248
block_273:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_250
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_250
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_292
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_292
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_249
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_249
block_274:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_275
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_275
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_293
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_293
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_251
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_251
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_253
block_275:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_276
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_276
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_295
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_295
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_274
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_274
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_254
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_254
block_276:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_277
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_277
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_296
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_296
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_275
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_275
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_255
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_255
block_277:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_278
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_278
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_297
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_297
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_276
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_276
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_256
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_256
block_278:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_298
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_298
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_277
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_277
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_257
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_257
block_279:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_280
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_350
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_383
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_383
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_367
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_278
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_258
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_258
block_280:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_281
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_281
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_299
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_299
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_259
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_259
block_281:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_282
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_282
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_300
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_300
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_280
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_280
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_260
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_260
block_282:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_283
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_283
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_301
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_301
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_281
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_281
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_261
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_261
block_283:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_190
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_302
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_302
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_282
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_282
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_262
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_262
block_284:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_285
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_285
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_304
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_304
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_263
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_263
block_285:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_286
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_286
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_305
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_305
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_284
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_284
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_264
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_264
block_286:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_287
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_319
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_335
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_335
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_318
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_285
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_265
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_265
block_287:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_220
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_306
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_306
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_286
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_286
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_266
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_266
block_289:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_290
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_290
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_309
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_309
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_269
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_269
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_270
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_270
block_290:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_291
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_291
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_310
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_310
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_289
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_289
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_271
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_271
block_291:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_311
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_311
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_290
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_290
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_272
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_272
block_292:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_250
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_250
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_250
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_250
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_273
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_273
block_293:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_294
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_294
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_294
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_294
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_251
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_251
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_274
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_274
block_294:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_295
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_295
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_329
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_328
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_312
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_312
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_205
block_295:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_296
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_296
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_347
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_347
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_329
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_294
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_275
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_275
block_296:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_297
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_298
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_348
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_295
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_295
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_276
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_276
block_297:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_298
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_298
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_296
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_296
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_277
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_277
block_298:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_349
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_349
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_296
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_297
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_278
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_278
block_299:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_300
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_300
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_313
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_313
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_280
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_280
block_300:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_301
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_301
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_314
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_314
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_299
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_299
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_281
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_281
block_301:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_302
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_302
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_315
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_315
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_300
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_300
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_282
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_282
block_302:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_303
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_333
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_352
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_352
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_332
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_301
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_283
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_283
block_303:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_239
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_316
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_316
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_302
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_302
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_190
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_190
block_304:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_305
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_305
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_317
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_317
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_284
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_284
block_305:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_286
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_286
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_318
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_318
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_304
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_304
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_285
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_285
block_306:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_307
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_307
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_319
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_319
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_286
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_286
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_287
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_287
block_307:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_267
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_267
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_320
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_320
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_306
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_306
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_220
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_220
block_308:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_269
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_269
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_322
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_322
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_267
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_267
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_268
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_268
block_309:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_310
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_310
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_323
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_323
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_269
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_269
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_289
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_289
block_310:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_311
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_311
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_324
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_324
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_309
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_309
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_290
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_290
block_311:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_325
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_325
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_310
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_310
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_291
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_291
block_312:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_294
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_328
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_345
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_345
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_250
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_251
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_251
block_313:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_314
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_314
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_330
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_330
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_299
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_299
block_314:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_315
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_315
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_331
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_331
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_313
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_313
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_300
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_300
block_315:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_302
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_302
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_332
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_332
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_314
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_314
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_301
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_301
block_316:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_239
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_333
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_333
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_302
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_302
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_303
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_303
block_317:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_318
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_318
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_354
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_354
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_304
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_304
block_318:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_286
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_286
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_334
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_334
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_317
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_317
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_305
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_305
block_319:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_320
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_388
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_403
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    jump block_403
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_387
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_286
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_306
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_306
block_320:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_321
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_321
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_336
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_336
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_319
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_307
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_307
block_321:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_322
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_322
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_337
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_337
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_320
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_320
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_267
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_267
block_322:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_269
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_269
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_338
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_338
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_321
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_321
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_308
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_308
block_323:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_324
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_324
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_340
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_340
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_269
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_269
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_309
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_309
block_324:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_325
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_325
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_342
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_342
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_323
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_323
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_310
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_310
block_325:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_343
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_343
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_324
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_324
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_311
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_311
block_326:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_327
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_327
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_364
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_364
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_250
block_327:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_329
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_379
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_391
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_391
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_344
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_326
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 6
    jump block_312
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 6
    jump block_312
block_328:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_329
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_329
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_346
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_346
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_312
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_312
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_294
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_294
block_329:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_295
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_365
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_379
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_379
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_327
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_327
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_294
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_294
block_330:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_331
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_331
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_350
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_350
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_313
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_313
block_331:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_332
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_384
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_398
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_398
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_350
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_330
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_314
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_314
block_332:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_302
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_302
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_351
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_351
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_331
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_331
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_315
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_315
block_333:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_239
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_353
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_353
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_302
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_302
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_316
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_316
block_334:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_335
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_335
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_355
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_318
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_318
block_335:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_319
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_319
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_373
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_373
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_334
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_334
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_286
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_286
block_336:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_337
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_337
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_357
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_357
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_319
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_320
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_320
block_337:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_338
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_338
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_358
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_358
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_336
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_336
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_321
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_321
block_338:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_339
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_339
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_359
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_359
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_337
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_337
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_322
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_322
block_339:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_340
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_340
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_360
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_360
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_338
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_338
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_269
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_269
block_340:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_341
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_341
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_361
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_361
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_339
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_339
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_323
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_323
block_341:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_342
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_342
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_362
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_362
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_340
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_340
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_175
block_342:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_343
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_343
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_363
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_363
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_341
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_341
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_324
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_324
block_343:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_408
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_408
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_363
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_342
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_325
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_325
block_344:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_327
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_391
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_409
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_409
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 6
    jump block_326
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 6
    jump block_326
block_345:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_346
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_346
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_327
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_327
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_327
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_327
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_312
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_312
block_346:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_329
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_329
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_329
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_329
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_345
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_345
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_328
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_328
block_347:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_348
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_365
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_365
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_329
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_329
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_295
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_295
block_348:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_349
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_349
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_366
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_366
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_347
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_347
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_296
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_296
block_349:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_367
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_367
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_348
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_298
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_298
block_350:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_331
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_331
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_397
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_397
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_383
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_330
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_330
block_351:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_352
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_352
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_368
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_368
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_331
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_331
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_332
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_332
block_352:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_353
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_353
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_369
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_369
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_351
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_351
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_302
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_302
block_353:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_239
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_370
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_352
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_352
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_333
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_333
block_354:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_355
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_355
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_372
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_372
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_317
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_317
block_355:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_319
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_439
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_462
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_462
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_438
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_354
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_334
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_334
block_357:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_358
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_358
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_374
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_374
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_319
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_336
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_336
block_358:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_359
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_359
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_375
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_375
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_357
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_357
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_337
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_337
block_359:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_360
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_360
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_376
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_376
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_358
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_358
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_338
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_338
block_360:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_361
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_361
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_377
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_377
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_359
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_359
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_339
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_339
block_361:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_362
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_406
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_422
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_422
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_377
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_360
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_340
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_340
block_362:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_363
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_363
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_378
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_378
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_361
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_341
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_341
block_363:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_343
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_343
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_407
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_407
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_390
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_362
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_342
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_342
block_364:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_344
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_344
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_344
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_344
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_326
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_326
block_365:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_366
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_366
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_380
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_380
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_329
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_329
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_347
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_347
block_366:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_367
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_367
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_381
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_381
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_365
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_365
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_348
block_367:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_382
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_382
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_366
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_366
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_349
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_349
block_368:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_369
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_369
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_384
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_384
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_331
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_331
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_351
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_351
block_369:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_370
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_370
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_385
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_385
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_368
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_368
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_352
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_352
block_370:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_371
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_417
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_436
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_436
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_400
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_369
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_353
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_353
block_371:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_372
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_372
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_386
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_386
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_239
block_372:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_355
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_355
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_438
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_438
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_417
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_371
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_354
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_354
block_373:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_319
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_319
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_387
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_387
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_335
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_335
block_374:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_375
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_375
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_388
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_388
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_319
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_357
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_357
block_375:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_376
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_376
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_389
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_389
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_374
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_374
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_358
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_358
block_376:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_377
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_421
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_442
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_442
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_420
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_359
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_359
block_377:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_361
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_361
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_421
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_421
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_376
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_376
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_360
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_360
block_378:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_363
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_363
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_390
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_390
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_361
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_362
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_362
block_379:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_380
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_380
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_470
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_392
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_327
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_327
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_329
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_329
block_380:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_381
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_381
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_393
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_393
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_379
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_379
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_365
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_365
block_381:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_382
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_382
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_394
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_394
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_380
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_380
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_366
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_366
block_382:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_383
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_383
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_395
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_395
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_381
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_381
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_367
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_367
block_383:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_350
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_350
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_396
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_396
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_382
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_382
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_279
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_279
block_384:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_385
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_385
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_399
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_399
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_331
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_331
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_368
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_368
block_385:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_370
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_370
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_400
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_400
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_384
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_384
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_369
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_369
block_386:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_372
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_372
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_401
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_401
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_371
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_371
block_387:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_319
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_319
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_402
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_402
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_373
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_373
block_388:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_389
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_389
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_404
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_404
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_319
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_374
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_374
block_389:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_376
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_376
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_405
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_405
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_388
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_388
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_375
block_390:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_363
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_363
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_406
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_406
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_361
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_378
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_378
block_391:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_392
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_410
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_427
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_409
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_344
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_327
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_327
block_392:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_393
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_393
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_410
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_410
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_391
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_379
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_379
block_393:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_394
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_394
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_411
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_411
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_392
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_392
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_380
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_380
block_394:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_395
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_395
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_412
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_412
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_393
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_393
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_381
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_381
block_395:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_396
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_396
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_413
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_413
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_394
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_394
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_382
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_382
block_396:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_397
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_397
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_414
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_414
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_395
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_395
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_383
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_383
block_397:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_398
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_398
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_415
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_415
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_396
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_396
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_350
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_350
block_398:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_399
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_399
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_416
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_416
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_397
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_397
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_331
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_331
block_399:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_400
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_436
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_457
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_457
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_434
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_398
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_384
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_384
block_400:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_370
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_370
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_458
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_458
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_399
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_399
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_385
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_385
block_401:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_372
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_372
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_417
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_417
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_386
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_386
block_402:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_403
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_403
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_418
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_418
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_387
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_387
block_403:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_404
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_404
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_419
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_419
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_402
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_402
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_319
block_404:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_405
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_464
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_464
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_440
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_403
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_388
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_388
block_405:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_376
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_376
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_420
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_420
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_404
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_404
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_389
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_389
block_406:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_407
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_407
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_423
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_423
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_361
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_390
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_390
block_407:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_408
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_408
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_424
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_424
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_406
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_406
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_363
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_363
block_408:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_425
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_425
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_407
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_407
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_343
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_343
block_409:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_391
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_426
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_449
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_448
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_344
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_344
block_410:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_411
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_411
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_428
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_428
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_391
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_392
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_392
block_411:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_412
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_412
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_429
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_429
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_410
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_410
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_393
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_393
block_412:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_413
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_413
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_430
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_430
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_411
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_411
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_394
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_394
block_413:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_414
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_414
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_431
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_431
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_412
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_412
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_395
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_395
block_414:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_415
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_415
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_432
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_432
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_413
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_413
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_396
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_396
block_415:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_416
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_416
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_433
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_433
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_414
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_414
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_397
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_397
block_416:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_399
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_399
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_434
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_434
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_415
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_415
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_398
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_398
block_417:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_372
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_372
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_437
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_437
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_401
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_401
block_418:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_419
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_419
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_439
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_439
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_402
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_402
block_419:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_404
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_404
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_440
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_440
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_418
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_418
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_403
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_403
block_420:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_376
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_376
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_441
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_404
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_404
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_405
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_405
block_421:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_422
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_422
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_443
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_443
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_376
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_376
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_377
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_377
block_422:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_423
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_423
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_444
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_444
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_421
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_421
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_361
block_423:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_424
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_424
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_445
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_422
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_422
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_406
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_406
block_424:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_425
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_425
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_446
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_446
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_423
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_423
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_407
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_407
block_425:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_447
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_447
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_424
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_424
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_408
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_408
block_426:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_489
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_508
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 7
    jump block_528
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 7
    jump block_528
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_449
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_409
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_391
block_427:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_428
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_428
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_450
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_391
block_428:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_429
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_429
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_451
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_451
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_427
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_427
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_410
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_410
block_429:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_430
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_430
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_452
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_452
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_428
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_428
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_411
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_411
block_430:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_431
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_472
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_492
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_492
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_471
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_429
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_412
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_412
block_431:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_432
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_432
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_453
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_453
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_430
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_430
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_413
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_413
block_432:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_433
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_433
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_454
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_454
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_431
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_431
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_414
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_414
block_433:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_434
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_434
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_455
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_455
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_432
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_432
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_415
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_415
block_434:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_399
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_399
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_456
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_456
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_433
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_433
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_416
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_416
block_436:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_437
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_437
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_459
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_459
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_399
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_399
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_370
block_437:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_438
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_438
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_460
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_460
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_436
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_436
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_417
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_417
block_438:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_355
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_355
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_461
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_461
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_437
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_437
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_372
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_372
block_439:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_440
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_463
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_500
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_500
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_481
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_418
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_418
block_440:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_404
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_404
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_463
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_439
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_439
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_419
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_419
block_441:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_442
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_521
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 7
    jump block_573
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 7
    jump block_573
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_557
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_404
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_420
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_420
block_442:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_443
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_443
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_465
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_465
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_441
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_376
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_376
block_443:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_444
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_444
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_484
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_484
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_442
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_442
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_421
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_421
block_444:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_466
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_466
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_443
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_443
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_422
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_422
block_445:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_446
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_505
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_523
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_523
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_504
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_444
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_423
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_423
block_446:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_447
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_447
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_467
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_467
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_424
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_424
block_447:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_468
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_468
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_446
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_446
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_425
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_425
block_448:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_449
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_449
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_469
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_469
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_409
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_409
block_449:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_426
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_426
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_527
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_527
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_409
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_409
block_450:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_451
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_470
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_426
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_427
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_427
block_451:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_452
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_452
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_470
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_470
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_450
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_428
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_428
block_452:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_430
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_430
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_471
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_471
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_451
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_451
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_429
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_429
block_453:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_454
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_454
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_472
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_472
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_430
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_430
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_431
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_431
block_454:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_455
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_455
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_473
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_473
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_453
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_453
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_432
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_432
block_455:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_456
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_456
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_474
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_474
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_454
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_454
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_433
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_433
block_456:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_457
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_457
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_475
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_475
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_455
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_455
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_434
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_434
block_457:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_458
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_458
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_456
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_456
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_399
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_399
block_458:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_459
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_459
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_477
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_477
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_457
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_457
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_400
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_400
block_459:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_460
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_460
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_478
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_478
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_458
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_458
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_436
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_436
block_460:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_461
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_461
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_497
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_497
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_459
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_459
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_437
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_437
block_461:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_462
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_462
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_480
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_480
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_460
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_460
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_438
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_438
block_462:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_439
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_439
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_481
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_481
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_461
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_461
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_355
block_463:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_464
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_557
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_571
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_571
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_556
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 6
    jump block_439
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_440
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_440
block_464:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_441
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_482
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_482
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_463
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_404
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_404
block_465:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_466
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_466
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_483
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_483
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_441
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_442
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_442
block_466:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_485
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_485
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_465
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_465
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_444
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_444
block_467:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_468
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_468
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_486
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_486
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_446
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_446
block_468:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_487
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_487
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_467
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_467
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_447
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_447
block_469:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_449
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_449
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_449
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_449
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_448
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_448
block_470:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_471
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_471
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_490
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_489
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_450
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_451
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_379
block_471:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_430
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_430
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_491
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_491
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_470
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_470
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_452
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_452
block_472:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_473
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_473
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_430
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_430
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_453
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_453
block_473:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_474
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_474
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_472
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_472
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_454
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_454
block_474:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_475
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_475
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_493
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_493
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_473
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_473
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_455
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_455
block_475:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_476
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_514
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_533
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_533
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_513
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_474
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_456
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_456
block_476:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_477
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_477
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_494
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_494
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_475
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_475
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_457
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_457
block_477:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_478
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_478
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_495
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_495
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_476
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_458
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_458
block_478:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_480
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_480
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_496
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_496
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_477
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_477
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_459
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_459
block_480:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_481
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_481
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_498
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_498
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_478
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_478
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_461
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_461
block_481:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_439
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_439
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_499
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_499
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_480
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_480
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_462
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_462
block_482:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_441
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_501
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_501
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_463
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_464
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_464
block_483:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_484
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_484
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_502
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_502
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_441
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_465
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_465
block_484:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_485
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_485
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_503
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_503
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_483
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_483
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_443
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_443
block_485:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_504
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_504
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_484
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_484
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_466
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_466
block_486:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_487
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_487
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_505
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_505
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_467
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_467
block_487:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_488
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_488
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_506
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_506
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_486
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_486
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_468
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_468
block_488:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_507
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_507
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_487
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_487
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_179
block_489:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_490
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_490
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_508
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_508
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_470
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_470
block_490:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_491
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_509
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_529
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_529
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_508
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_489
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_470
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_470
block_491:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_492
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_492
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_509
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_509
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_490
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_490
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_471
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_471
block_492:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_493
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_493
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_510
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_510
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_491
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_491
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_430
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_430
block_493:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_475
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_475
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_513
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_513
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_492
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_492
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_474
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_474
block_494:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_495
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_495
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_514
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_514
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_475
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_475
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_476
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_476
block_495:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_496
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_496
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_515
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_515
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_494
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_494
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_477
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_477
block_496:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_497
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_497
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_516
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_516
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_495
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_495
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_478
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_478
block_497:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_498
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_498
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_517
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_517
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_496
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_496
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_460
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_460
block_498:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_499
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_499
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_518
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_518
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_497
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_497
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_480
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_480
block_499:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_500
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_539
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_555
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_555
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_538
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_498
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_481
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_481
block_500:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_463
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_463
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_519
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_519
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_499
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_499
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_439
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_439
block_501:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_441
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_520
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_520
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_463
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_482
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_482
block_502:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_503
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_503
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_521
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_521
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_441
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_483
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_483
block_503:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_504
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_504
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_522
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_522
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_502
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_502
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_484
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_484
block_504:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_558
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_575
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_575
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_541
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_503
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_485
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_485
block_505:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_506
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_506
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_524
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_524
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_486
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_486
block_506:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_507
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_507
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_525
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_525
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_505
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_505
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_487
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_487
block_507:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_526
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_526
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_506
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_506
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_488
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_488
block_508:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_490
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_490
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_489
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_489
block_509:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_510
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_510
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_530
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_530
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_490
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_490
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_491
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_491
block_510:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_531
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_531
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_509
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_509
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_492
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_492
block_511:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_939
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_510
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_472
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_472
block_512:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_513
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_940
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_473
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_473
block_513:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_475
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_475
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_532
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_532
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_493
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_493
block_514:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_515
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_515
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_534
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_534
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_475
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_475
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_494
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_494
block_515:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_516
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_516
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_535
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_535
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_514
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_514
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_495
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_495
block_516:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_517
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_517
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_536
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_536
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_515
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_515
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_496
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_496
block_517:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_518
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_518
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_537
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_537
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_516
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_516
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_497
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_497
block_518:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_499
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_499
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_538
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_538
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_517
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_517
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_498
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_498
block_519:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_463
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_463
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_539
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_539
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_499
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_499
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_500
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_500
block_520:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_441
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_540
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_540
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_463
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_501
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_501
block_521:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_522
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_541
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_574
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_574
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_441
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_502
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_502
block_522:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_504
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_504
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_541
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_541
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_521
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_521
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_503
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_503
block_523:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_524
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_524
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_542
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_542
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_504
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_504
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_445
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_445
block_524:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_525
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_525
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_543
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_543
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_523
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_523
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_505
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_505
block_525:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_526
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_526
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_544
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_544
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_524
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_524
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_506
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_506
block_526:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_545
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_545
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_525
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_525
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_507
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_507
block_527:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_528
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_528
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_562
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_562
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_449
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_449
block_528:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_529
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_547
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_563
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_563
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_527
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_527
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_426
block_529:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_530
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_530
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_547
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_547
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_528
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_528
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_490
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_490
block_530:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_531
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_531
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_548
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_548
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_529
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_529
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_509
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_509
block_531:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_549
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_549
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_530
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_530
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_510
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_510
block_532:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_533
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_533
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_550
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_550
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_513
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_513
block_533:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_534
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_534
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_551
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_551
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_532
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_532
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_475
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_475
block_534:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_535
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_535
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_552
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_552
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_533
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_533
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_514
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_514
block_535:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_536
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_536
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_553
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_553
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_534
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_534
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_515
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_515
block_536:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_537
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_537
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_554
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_554
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_535
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_535
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_516
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_516
block_537:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_538
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_587
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_604
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_604
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_554
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_536
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_517
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_517
block_538:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_499
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_570
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_587
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_587
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_537
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_537
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_518
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_518
block_539:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_463
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_463
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_556
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_556
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_499
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_499
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_519
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_519
block_540:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_441
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_557
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_557
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_463
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_520
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_520
block_541:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_504
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_575
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_611
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_611
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_592
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_521
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_522
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_522
block_542:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_543
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_543
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_558
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_558
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_504
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_504
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_523
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_523
block_543:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_544
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_544
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_559
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_559
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_542
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_542
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_524
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_524
block_544:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_545
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_545
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_561
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_561
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_543
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_543
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_525
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_525
block_545:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_614
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_614
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_596
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_544
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_526
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_526
block_547:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_548
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_548
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_564
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_564
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_528
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_528
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_529
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_529
block_548:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_549
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_549
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_565
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_565
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_547
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_547
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_530
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_530
block_549:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_566
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_566
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_548
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_548
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_531
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_531
block_550:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_551
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_585
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_602
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_602
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_532
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_532
block_551:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_552
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_552
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_567
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_567
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_550
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_550
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_533
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_533
block_552:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_553
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_553
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_568
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_568
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_551
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_551
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_534
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_534
block_553:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_554
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_554
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_569
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_569
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_552
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_552
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_535
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_535
block_554:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_537
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_619
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_635
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    jump block_635
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_586
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_553
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_536
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_536
block_555:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_556
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_556
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_570
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_570
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_538
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_538
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_499
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_499
block_556:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_463
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_639
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_654
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_654
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_638
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_555
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 6
    jump block_539
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 6
    jump block_539
block_557:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_441
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_572
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_572
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_463
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_540
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_540
block_558:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_559
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_559
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_576
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_576
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_504
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_504
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_542
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_542
block_559:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_560
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_560
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_577
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_577
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_558
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_558
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_543
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_543
block_560:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_561
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_561
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_578
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_578
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_559
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_559
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_226
block_561:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_545
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_545
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_579
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_579
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_560
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_560
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_544
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_544
block_562:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_563
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_563
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_664
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_580
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_527
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_527
block_563:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_564
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_564
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_581
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_664
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_562
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_562
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_528
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_528
block_564:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_565
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_565
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_582
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_582
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_563
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_563
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_547
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_547
block_565:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_566
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_566
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_583
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_583
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_564
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_564
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_548
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_548
block_566:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_584
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_584
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_565
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_565
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_549
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_549
block_567:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_568
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_568
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_585
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_585
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_550
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_550
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_551
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_551
block_568:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_569
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_586
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_634
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_634
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_618
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_567
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_552
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_552
block_569:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_554
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_554
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_586
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_586
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_568
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_568
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_553
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_553
block_570:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_556
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_556
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_588
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_588
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_538
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_538
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_555
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_555
block_571:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_572
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_572
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_589
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_589
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_556
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_556
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_463
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_463
block_572:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_573
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_573
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_590
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_590
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_571
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_571
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_557
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_557
block_573:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_574
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_574
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_591
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_591
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_572
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_572
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_441
block_574:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_541
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_541
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_592
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_592
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_573
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_573
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_521
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_521
block_575:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_576
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_593
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_627
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_627
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_611
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_541
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_504
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_504
block_576:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_577
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_577
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_593
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_593
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_575
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_575
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_558
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_558
block_577:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_578
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_578
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_594
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_594
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_576
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_576
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_559
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_559
block_578:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_579
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_579
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_595
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_595
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_577
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_577
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_560
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_560
block_579:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_545
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_545
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_596
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_596
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_578
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_578
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_561
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_561
block_580:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_581
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_581
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_597
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_597
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_562
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_562
block_581:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_582
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_582
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_598
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_598
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_580
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_580
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_563
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_563
block_582:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_583
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_583
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_599
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_599
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_581
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_581
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_564
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_564
block_583:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_584
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_584
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_600
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_600
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_582
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_582
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_565
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_565
block_584:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_601
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_601
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_583
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_583
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_566
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_566
block_585:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_568
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_568
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_618
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_618
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_550
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_550
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_567
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_567
block_586:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_554
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_635
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_649
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_649
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_634
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_568
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_569
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_569
block_587:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_588
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_588
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_605
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_605
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_537
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_537
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_538
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_538
block_588:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_556
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_556
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_606
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_606
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_587
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_587
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_570
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_570
block_589:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_590
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_590
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_607
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_607
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_556
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_556
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_571
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_571
block_590:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_591
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_591
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_608
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_608
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_589
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_589
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_572
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_572
block_591:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_592
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_592
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_609
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_609
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_590
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_590
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_573
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_573
block_592:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_541
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_541
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_610
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_610
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_591
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_591
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_574
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_574
block_593:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_594
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_612
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_644
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_644
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_627
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_575
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_576
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_576
block_594:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_595
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_595
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_612
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_612
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_593
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_593
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_577
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_577
block_595:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_596
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_596
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_613
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_613
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_594
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_594
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_578
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_578
block_596:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_545
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_645
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_662
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_662
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_613
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_595
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_579
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_579
block_597:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_598
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_598
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_615
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_615
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_580
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_580
block_598:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_599
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_599
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_616
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_616
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_597
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_597
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_581
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_581
block_599:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_600
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_600
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_598
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_598
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_582
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_582
block_600:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_601
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_601
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_665
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_665
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_599
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_583
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_583
block_601:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_686
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_686
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_600
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_600
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_584
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_584
block_602:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_568
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_568
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_617
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_617
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_550
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_550
block_604:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_605
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_605
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_619
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_619
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_554
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_554
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_537
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_537
block_605:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_606
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_606
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_620
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_620
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_604
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_604
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_587
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_587
block_606:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_556
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_556
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_621
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_621
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_605
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_605
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_588
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_588
block_607:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_608
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_608
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_622
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_622
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_556
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_556
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_589
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_589
block_608:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_609
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_609
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_623
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_623
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_607
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_607
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_590
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_590
block_609:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_610
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_610
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_624
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_624
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_608
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_608
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_591
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_591
block_610:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_611
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_611
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_625
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_625
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_609
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_609
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_592
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_592
block_611:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_575
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_575
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_626
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_626
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_610
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_610
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_541
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_541
block_612:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_613
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_699
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_716
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_716
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_681
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_593
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_594
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_594
block_613:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_596
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_683
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_717
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_717
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_682
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 5
    jump block_595
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 5
    jump block_595
block_614:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_628
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_628
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_596
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_596
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_545
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_545
block_615:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_616
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_616
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_629
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_629
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_597
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_597
block_616:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_600
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_600
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_630
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_630
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_615
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_615
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_598
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_598
block_617:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_618
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_618
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_632
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_632
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_602
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_602
block_618:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_568
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_568
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_633
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_633
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_617
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_617
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_585
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_585
block_619:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_620
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_620
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_636
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_636
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_554
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_554
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_604
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_604
block_620:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_621
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_621
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_637
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_637
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_619
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_619
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_605
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_605
block_621:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_556
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_556
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_638
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_638
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_620
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_620
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_606
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_606
block_622:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_623
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_623
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_639
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_639
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_556
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_556
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_607
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_607
block_623:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_624
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_624
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_640
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_640
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_622
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_622
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_608
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_608
block_624:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_625
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_657
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_676
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_676
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_656
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_623
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_609
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_609
block_625:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_626
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_626
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_641
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_641
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_624
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_624
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_610
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_610
block_626:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_627
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_627
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_642
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_642
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_625
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_625
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_611
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_611
block_627:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_593
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_593
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_643
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_643
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_626
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_626
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_575
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_575
block_628:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_645
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_645
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_596
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_596
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_614
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_614
block_629:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_630
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_630
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_664
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_664
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_615
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_615
block_630:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_664
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_664
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_629
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_629
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_616
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_616
block_631:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_600
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_600
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_937
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_630
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_508
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_599
block_632:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_633
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_633
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_646
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_646
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_617
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_617
block_633:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_634
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_634
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_647
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_647
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_632
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_632
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_618
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_618
block_634:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_586
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_586
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_648
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_648
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_633
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_633
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_568
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_568
block_635:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_636
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_636
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_650
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_650
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_586
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_586
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_554
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_554
block_636:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_637
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_637
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_651
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_651
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_635
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_635
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_619
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_619
block_637:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_638
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_638
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_652
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_652
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_636
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_636
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_620
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_620
block_638:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_556
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_556
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_653
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_653
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_637
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_637
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_621
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_621
block_639:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_640
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_640
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_655
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_655
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_556
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_556
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_622
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_622
block_640:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_624
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_624
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_656
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_656
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_639
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_639
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_623
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_623
block_641:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_642
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_642
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_657
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_657
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_624
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_624
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_625
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_625
block_642:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_643
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_643
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_658
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_658
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_641
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_641
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_626
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_626
block_643:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_644
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_644
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_659
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_659
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_642
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_642
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_627
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_627
block_644:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_612
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_612
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_660
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_660
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_643
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_643
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_593
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_593
block_645:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_663
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_663
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_596
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_596
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_628
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_628
block_646:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_647
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_647
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_666
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_666
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_632
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_632
block_647:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_648
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_648
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_667
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_667
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_646
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_646
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_633
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_633
block_648:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_649
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_649
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_668
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_668
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_647
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_647
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_634
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_634
block_649:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_650
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_650
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_669
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_669
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_648
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_648
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_586
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_586
block_650:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_651
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_651
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_670
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_670
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_649
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_649
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_635
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_635
block_651:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_652
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_652
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_671
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_671
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_650
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_650
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_636
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_636
block_652:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_653
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_653
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_672
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_672
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_651
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_651
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_637
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_637
block_653:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_654
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_654
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_673
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_673
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_652
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_652
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_638
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_638
block_654:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_655
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_655
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_674
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_674
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_653
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_653
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_556
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_556
block_655:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_656
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_656
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_675
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_675
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_654
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_654
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_639
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_639
block_656:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_624
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_696
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_712
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_712
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_695
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_655
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_640
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_640
block_657:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_658
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_658
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_677
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_677
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_624
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_624
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_641
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_641
block_658:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_659
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_659
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_678
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_678
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_657
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_657
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_642
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_642
block_659:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_660
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_660
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_680
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_679
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_658
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_658
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_643
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_93
block_660:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_612
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_612
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_681
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_681
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_659
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_659
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_644
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_644
block_661:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_613
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_613
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_682
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_682
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_612
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_226
block_662:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_663
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_663
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_683
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_683
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_613
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_613
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_596
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_596
block_663:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_684
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_684
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_662
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_662
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_645
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_645
block_664:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_701
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    jump block_701
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_629
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_630
block_665:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_685
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_685
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_600
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_600
block_666:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_667
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_667
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_687
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_687
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_646
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_646
block_667:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_668
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_668
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_688
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_688
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_666
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_666
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_647
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_647
block_668:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_669
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_669
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_689
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_689
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_667
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_667
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_648
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_648
block_669:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_670
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_670
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_690
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_690
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_668
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_668
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_649
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_649
block_670:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_671
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_671
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_691
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_691
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_669
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_669
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_650
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_650
block_671:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_672
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_672
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_692
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_692
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_670
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_670
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_651
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_651
block_672:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_673
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_673
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_693
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_693
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_671
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_671
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_652
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_652
block_673:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_674
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_726
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_743
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_743
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_725
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_672
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_653
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_653
block_674:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_675
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_675
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_694
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_694
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_673
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_673
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_654
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_654
block_675:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_656
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_656
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_695
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_695
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_674
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_674
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_655
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_655
block_676:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_677
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_677
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_696
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_696
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_656
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_656
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_624
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_624
block_677:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_678
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_678
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_697
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_697
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_676
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_676
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_657
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_657
block_678:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_679
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_679
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_698
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_698
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_677
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_677
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_658
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_658
block_679:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_680
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_680
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_715
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_715
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_678
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_678
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_659
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_659
block_680:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_681
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_681
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_715
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_715
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_679
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_679
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_659
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_659
block_681:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_612
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_731
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_750
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_750
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_715
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_680
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_660
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_660
block_682:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_613
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_613
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_717
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_717
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_612
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_661
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_661
block_683:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_684
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_684
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_699
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_699
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_613
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_613
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_662
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_662
block_684:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_700
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_700
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_683
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_683
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_663
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_663
block_685:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_686
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_686
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_703
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_703
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_665
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_665
block_686:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_704
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_704
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_685
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_685
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_601
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_601
block_687:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_688
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_688
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_705
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_705
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_666
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_666
block_688:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_689
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_689
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_706
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_706
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_687
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_687
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_667
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_667
block_689:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_690
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_690
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_707
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_707
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_688
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_688
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_668
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_668
block_690:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_691
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_691
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_708
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_708
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_689
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_689
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_669
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_669
block_691:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_692
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_760
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_788
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_788
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_759
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_690
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_670
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_670
block_692:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_693
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_693
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_709
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_709
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_691
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_691
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_671
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_671
block_693:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_673
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_673
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_710
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_710
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_692
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_692
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_672
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_672
block_694:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_695
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_695
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_711
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_711
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_673
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_673
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_674
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_674
block_695:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_656
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_745
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_763
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_763
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_744
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_694
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_675
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_675
block_696:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_697
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_697
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_713
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_713
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_656
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_656
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_676
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_676
block_697:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_698
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_698
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_714
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_714
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_696
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_696
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_677
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_677
block_698:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_681
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_715
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_748
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_748
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_729
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_697
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_678
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_678
block_699:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_700
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_700
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_718
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_718
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_612
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_683
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_683
block_700:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_734
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_734
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_699
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_699
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_684
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_684
block_701:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_702
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_702
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_702
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_702
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_664
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_664
block_702:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_735
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_735
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_664
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_664
block_703:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_704
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_704
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_719
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_719
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_685
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_685
block_704:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_720
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_720
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_703
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_703
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_686
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_686
block_705:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_706
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_706
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_721
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_721
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_687
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_687
block_706:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_707
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_707
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_722
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_722
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_705
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_705
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_688
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_688
block_707:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_708
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_708
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_723
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_723
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_706
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_706
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_689
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_689
block_708:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_691
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_691
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_724
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_724
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_707
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_707
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_690
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_690
block_709:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_710
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_710
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_725
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_725
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_691
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_691
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_692
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_692
block_710:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_673
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_673
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_709
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_709
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_693
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_693
block_711:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_695
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_695
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_726
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_726
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_673
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_673
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_694
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_694
block_712:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_713
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_713
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_727
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_727
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_695
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_695
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_656
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_656
block_713:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_714
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_714
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_728
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_728
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_712
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_712
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_696
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_696
block_714:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_698
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_698
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_729
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_729
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_713
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_713
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_697
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_697
block_715:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_681
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_681
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_749
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_749
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_698
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_698
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_679
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_680
block_716:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_717
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_717
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_731
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_731
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_681
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_681
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_612
block_717:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_718
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_718
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_732
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_716
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_716
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_682
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_613
block_718:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_733
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_733
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_717
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_717
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_699
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_699
block_719:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_720
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_720
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_736
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_736
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_703
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_703
block_720:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_737
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_737
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_719
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_719
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_704
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_704
block_721:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_722
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_722
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_738
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_738
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_705
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_705
block_722:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_723
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_723
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_739
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_739
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_721
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_721
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_706
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_706
block_723:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_724
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_724
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_740
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_740
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_722
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_722
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_707
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_707
block_724:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_691
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_691
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_741
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_741
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_723
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_723
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_708
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_708
block_725:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_673
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_673
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_742
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_742
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_691
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_691
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_709
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_709
block_726:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_695
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_695
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_744
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_744
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_673
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_673
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_711
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_711
block_727:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_728
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_728
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_745
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_745
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_695
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_695
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_712
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_712
block_728:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_729
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_729
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_746
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_746
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_727
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_727
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_713
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_713
block_729:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_698
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_698
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_766
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_766
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_728
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_728
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_714
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_714
block_731:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_732
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_732
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_751
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_751
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_681
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_681
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_716
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_716
block_732:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_733
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_733
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_752
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_752
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_731
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_731
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_717
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_717
block_733:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_734
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_753
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_799
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_799
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_784
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_732
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_718
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_718
block_734:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_753
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_753
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_733
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_733
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_700
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_700
block_735:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_785
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_754
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_702
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_702
block_736:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_737
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_737
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_755
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_755
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_719
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_719
block_737:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_756
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_756
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_736
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_736
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_720
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_720
block_738:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_739
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_739
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_757
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_757
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_721
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_721
block_739:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_740
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_740
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_758
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_758
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_738
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_722
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_722
block_740:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_741
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_759
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_787
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_787
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_774
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_739
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_723
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_723
block_741:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_691
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_691
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_759
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_759
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_740
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_740
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_724
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_724
block_742:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_743
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_743
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_760
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_760
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_691
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_691
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_725
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_725
block_743:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_744
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_744
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_761
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_761
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_742
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_742
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_673
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_673
block_744:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_695
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_695
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_762
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_762
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_743
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_743
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_726
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_726
block_745:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_746
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_746
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_777
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_777
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_695
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_695
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_727
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_727
block_746:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_748
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_748
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_765
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_765
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_745
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_745
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_728
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_728
block_748:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_749
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_767
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_780
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_780
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_766
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_746
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_698
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_698
block_749:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_750
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_750
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_767
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_767
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_748
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_748
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_715
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_715
block_750:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_751
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_751
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_768
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_768
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_749
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_749
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_681
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_681
block_751:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_752
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_752
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_769
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_769
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_750
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_750
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_731
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_731
block_752:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_733
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_733
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_770
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_770
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_751
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_751
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_732
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_732
block_753:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_800
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_800
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_733
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_733
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_734
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_734
block_754:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_771
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_771
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_735
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_735
block_755:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_756
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_756
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_772
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_772
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_736
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_736
block_756:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_801
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_801
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_772
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_755
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_737
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_737
block_757:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_758
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_758
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_773
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_773
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_738
block_758:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_740
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_740
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_774
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_774
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_757
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_757
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_739
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_739
block_759:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_691
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_788
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_803
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_803
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_787
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_740
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_741
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_741
block_760:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_761
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_789
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_805
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_805
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_788
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_691
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_742
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_742
block_761:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_762
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_762
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_775
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_775
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_760
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_760
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_743
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_743
block_762:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_763
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_807
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_824
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_824
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_806
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_761
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_744
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_744
block_763:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_765
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_765
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_776
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_776
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_762
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_762
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_695
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_695
block_765:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_766
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_766
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_778
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_778
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_763
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_763
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_746
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_746
block_766:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_748
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_748
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_779
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_779
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_765
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_765
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_729
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_729
block_767:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_768
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_768
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_781
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_781
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_748
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_748
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_749
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_749
block_768:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_769
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_769
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_782
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_782
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_767
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_767
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_750
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_750
block_769:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_770
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_770
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_783
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_783
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_768
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_768
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_751
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_751
block_770:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_733
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_733
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_784
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_784
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_769
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_769
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_752
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_752
block_771:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_785
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_785
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_754
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_754
block_772:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    jump block_756
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_849
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_864
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_864
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_755
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_755
block_773:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_774
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_774
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_818
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_818
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_757
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_757
block_774:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_740
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_819
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_836
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_836
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_818
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_773
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_758
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_758
block_775:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_762
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_762
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_789
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_789
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_760
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_760
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_761
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_761
block_776:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_777
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_777
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_790
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_790
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_762
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_762
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_763
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_763
block_777:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_778
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_778
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_791
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_791
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_776
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_776
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_745
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_745
block_778:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_779
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_779
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_792
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_792
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_777
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_777
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_765
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_765
block_779:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_780
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_780
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_793
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_793
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_778
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_778
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_766
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_766
block_780:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_781
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_781
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_794
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_794
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_779
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_779
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_748
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_748
block_781:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_782
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_782
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_795
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_795
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_780
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_780
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_767
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_767
block_782:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_783
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_783
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_796
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_796
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_781
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_781
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_768
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_768
block_783:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_784
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_784
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_797
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_797
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_782
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_782
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_769
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_769
block_784:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_733
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_733
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_798
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_798
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_783
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_783
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_770
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_770
block_785:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_786
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_786
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_786
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_786
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_771
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_735
block_786:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_816
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_816
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_664
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_664
block_787:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_759
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_759
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_819
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_819
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_774
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_774
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_740
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_740
block_788:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_760
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_760
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_804
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_804
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_759
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_759
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_691
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_691
block_789:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_762
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_762
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_806
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_806
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_760
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_760
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_775
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_775
block_790:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_791
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_791
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_807
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_807
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_762
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_762
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_776
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_776
block_791:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_792
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_792
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_808
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_808
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_790
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_790
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_777
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_777
block_792:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_793
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_793
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_809
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_809
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_791
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_791
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_778
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_778
block_793:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_794
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_857
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_873
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_873
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_809
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_792
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 5
    jump block_779
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 5
    jump block_779
block_794:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_795
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_795
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_810
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_810
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_793
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_793
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_780
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_780
block_795:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_796
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_796
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_811
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_811
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_794
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_794
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_781
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_781
block_796:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_797
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_797
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_812
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_812
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_795
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_795
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_782
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_782
block_797:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_798
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_813
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_828
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_828
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_812
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_796
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_783
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_783
block_798:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_799
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_799
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_813
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_813
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_797
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_797
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_784
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_784
block_799:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_800
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_800
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_814
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_814
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_798
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_798
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_733
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_733
block_800:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_815
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_815
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_799
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_799
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_753
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_753
block_801:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_817
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_817
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_772
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_772
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_756
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_756
block_803:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_804
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_804
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_820
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_820
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_774
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_774
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_759
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_759
block_804:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_805
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_805
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_821
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_821
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_803
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_803
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_788
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_788
block_805:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_806
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_806
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_822
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_822
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_804
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_804
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_760
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_760
block_806:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_762
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_762
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_823
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_823
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_805
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_805
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_789
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_789
block_807:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_808
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_842
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_856
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_856
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_841
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_762
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_790
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_790
block_808:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_809
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_809
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_825
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_825
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_807
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_807
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_791
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_791
block_809:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_793
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_793
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_872
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_872
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_842
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_808
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_792
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_792
block_810:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_811
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_811
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_826
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_826
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_793
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_793
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_794
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_794
block_811:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_812
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_858
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_875
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_875
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_857
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_810
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_795
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_795
block_812:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_797
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_797
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_827
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_827
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_811
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_811
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_796
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_796
block_813:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_814
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_814
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_829
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_829
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_797
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_797
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_798
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_798
block_814:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_815
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_815
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_830
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_830
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_813
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_813
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_799
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_799
block_815:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_831
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_831
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_814
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_814
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_800
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_800
block_816:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_833
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_832
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_786
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_786
block_817:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_834
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_834
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_772
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_772
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_801
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_801
block_818:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_774
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_774
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_835
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_835
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_773
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_773
block_819:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_820
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_820
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_837
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_837
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_774
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_774
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_787
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_787
block_820:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_821
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_821
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_838
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_838
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_819
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_819
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_803
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_803
block_821:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_822
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_854
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_869
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_869
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_853
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_820
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_804
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_804
block_822:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_823
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_823
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_839
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_839
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_821
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_821
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_805
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_805
block_823:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_824
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_824
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_840
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_840
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_822
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_822
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_806
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_806
block_824:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_807
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_807
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_841
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_841
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_823
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_823
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_762
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_762
block_825:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_809
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_809
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_842
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_842
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_807
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_807
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_808
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_808
block_826:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_811
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_811
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_843
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_843
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_793
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_793
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_810
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_810
block_827:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_828
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_828
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_844
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_844
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_811
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_811
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_812
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_812
block_828:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_829
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_829
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_845
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_845
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_827
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_827
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_797
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_797
block_829:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_830
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_846
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_860
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_860
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_845
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_828
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_813
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_813
block_830:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_831
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_831
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_846
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_846
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_829
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_829
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_814
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_814
block_831:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_847
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_847
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_830
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_830
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_815
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_815
block_832:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_833
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_833
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_848
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_848
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_816
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_816
block_833:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_863
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    jump block_863
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_816
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_816
block_834:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_849
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_849
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_772
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_772
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_817
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_817
block_835:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_836
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_836
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_850
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_850
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_818
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_818
block_836:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_837
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_837
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_851
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_851
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_835
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_835
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_774
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_774
block_837:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_838
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_838
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_852
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_852
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_836
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_836
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_819
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_819
block_838:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_821
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_821
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_853
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_853
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_837
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_837
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_820
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_820
block_839:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_840
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_840
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_854
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_854
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_821
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_821
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_822
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_822
block_840:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_841
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_871
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_885
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_885
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_870
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_839
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_823
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_823
block_841:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_807
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_807
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_871
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_871
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_840
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_840
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_824
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_824
block_842:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_809
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_872
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_900
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_900
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_887
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_807
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_825
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_825
block_843:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_811
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_811
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_857
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_857
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_793
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_793
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_826
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_826
block_844:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_845
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_845
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_858
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_858
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_811
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_811
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_827
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_827
block_845:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_829
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_829
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_859
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_859
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_844
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_844
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_828
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_828
block_846:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_847
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_847
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_861
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_861
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_829
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_829
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_830
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_830
block_847:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_862
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_862
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_846
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_846
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_831
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_831
block_848:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_833
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_833
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_833
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_833
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_832
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_832
block_849:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_865
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_865
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_772
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_772
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_834
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_834
block_850:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_851
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_881
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_893
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_893
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_835
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_835
block_851:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_852
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_852
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_866
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_866
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_850
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_850
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_836
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_836
block_852:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_853
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_853
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_867
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_867
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_851
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_851
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_837
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_837
block_853:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_821
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_821
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_868
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_868
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_852
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_852
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_838
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_838
block_854:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_840
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_840
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_870
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_870
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_821
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_821
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_839
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_839
block_856:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_842
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_842
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_887
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_887
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_871
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_840
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_807
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_807
block_857:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_811
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_811
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_874
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_874
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_793
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_793
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_843
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_843
block_858:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_859
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_859
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_876
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_876
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_811
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_811
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_844
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_844
block_859:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_860
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_860
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_877
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_877
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_858
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_858
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_845
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_845
block_860:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_861
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_861
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_878
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_878
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_859
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_859
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_829
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_829
block_861:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_862
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_862
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_879
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_879
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_860
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_860
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_846
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_846
block_862:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_880
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_880
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_861
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_861
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_847
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_847
block_863:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_892
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_833
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_833
block_864:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_865
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_865
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_906
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_906
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_772
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_772
block_865:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_907
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_907
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_864
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_864
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_849
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_849
block_866:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_867
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_867
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_881
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_881
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_850
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_850
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_851
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_851
block_867:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_868
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_896
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_908
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_908
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_894
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_866
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_852
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_852
block_868:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_869
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_869
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_882
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_882
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_867
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_867
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_853
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_853
block_869:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_870
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_870
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_883
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_883
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_868
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_868
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_821
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_821
block_870:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_840
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_840
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_884
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_884
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_869
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_869
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_854
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_854
block_871:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_856
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_856
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_886
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_886
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_840
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_840
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_841
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_841
block_872:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_873
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_901
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_914
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_914
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_900
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_842
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_809
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_809
block_873:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_874
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_874
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_888
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_888
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_872
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_872
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_793
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_793
block_874:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_875
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_875
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_873
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_873
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_857
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_857
block_875:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_876
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_902
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_916
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_916
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_901
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_874
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_811
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_811
block_876:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_877
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_877
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_889
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_889
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_875
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_875
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_858
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_858
block_877:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_878
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_903
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_918
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_902
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_876
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_859
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_859
block_878:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_879
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_879
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_890
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_890
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_877
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_877
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_860
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_860
block_879:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_880
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_904
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_920
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_920
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_903
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_878
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_861
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_861
block_880:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_891
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_891
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_879
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_879
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_862
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_862
block_881:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_867
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_867
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_894
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_894
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_850
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_850
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_866
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_866
block_882:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_883
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_883
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_909
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_909
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_867
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_867
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_868
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_868
block_883:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_884
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_884
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_926
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_926
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_882
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_882
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_869
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_869
block_884:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_885
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_885
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_896
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_896
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_883
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_883
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_870
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_870
block_885:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_886
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_886
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_897
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_897
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_884
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_884
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_840
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_840
block_886:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_887
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_887
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_898
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_898
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_885
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_885
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_871
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_871
block_887:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_842
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_842
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_899
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_899
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_886
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_886
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_856
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_856
block_888:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_875
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_875
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_901
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_901
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_872
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_872
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_873
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_873
block_889:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_877
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_877
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_902
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_902
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_875
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_875
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_876
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_876
block_890:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_879
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_879
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_903
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_903
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_877
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_877
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_878
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_878
block_891:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_904
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_904
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_879
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_879
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_880
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_880
block_892:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_905
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_905
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_863
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_863
block_893:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_894
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_894
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_923
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_923
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_850
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_850
block_894:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_867
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_924
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_941
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_941
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_923
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_893
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_881
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_881
block_896:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_897
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_945
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_944
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_867
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_884
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_884
block_897:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_898
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_898
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_910
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_910
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_896
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_896
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_885
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_885
block_898:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_899
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_899
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_911
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_911
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_897
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_897
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_886
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_886
block_899:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_900
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_900
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_912
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_912
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_898
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_898
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_887
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_887
block_900:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_872
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_872
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_913
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_913
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_899
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_899
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_842
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_842
block_901:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_875
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_875
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_915
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_915
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_872
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_872
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_888
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_888
block_902:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_877
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_877
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_917
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_917
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_875
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_875
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_889
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_889
block_903:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_879
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_879
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_919
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_919
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_877
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_877
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_890
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_890
block_904:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_955
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_955
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_936
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_879
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_891
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_891
block_905:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_937
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_937
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_892
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_892
block_906:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_907
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_907
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_921
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_921
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_864
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_864
block_907:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_922
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_922
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_906
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_906
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_865
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_865
block_908:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_909
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_909
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_924
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_924
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_894
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_894
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_867
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_867
block_909:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_896
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_896
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_925
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_925
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_908
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_908
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_882
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_882
block_910:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_911
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_911
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_927
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_927
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_896
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_896
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_897
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_897
block_911:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_912
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_912
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_928
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_928
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_910
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_910
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_898
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_898
block_912:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_913
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_913
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_929
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_929
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_911
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_911
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_899
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_899
block_913:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_914
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_914
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_930
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_930
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_912
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_912
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_900
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_900
block_914:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_915
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_915
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_931
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_931
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_913
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_913
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_872
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_872
block_915:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_916
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_916
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_932
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_932
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_914
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_914
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_901
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_901
block_916:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_917
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_917
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_933
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_933
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_915
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_915
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_875
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_875
block_917:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_918
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_918
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_934
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_934
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_916
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_916
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_902
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_902
block_918:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_919
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_919
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_935
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_935
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_917
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_917
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_877
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_877
block_919:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_920
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_954
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_953
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_918
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_903
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_903
block_920:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_904
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_904
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_936
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_936
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_919
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_919
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_879
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_879
block_921:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_922
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_922
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_938
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_938
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_906
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_906
block_922:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_939
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_939
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_921
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_921
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_907
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_907
block_923:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_894
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_894
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_940
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_940
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_893
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_893
block_924:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_925
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_925
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_942
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_942
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_894
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_894
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_908
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_908
block_925:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_926
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_926
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_943
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_943
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_924
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_924
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_909
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_909
block_926:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_896
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_896
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_944
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_944
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_925
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_925
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_883
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_883
block_927:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_928
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_928
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_945
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_945
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_896
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_896
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_910
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_910
block_928:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_929
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_929
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_946
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_946
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_927
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_927
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_911
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_911
block_929:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_930
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_930
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_947
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_947
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_928
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_928
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_912
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_912
block_930:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_931
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_931
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_948
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_948
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_929
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_929
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_913
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_913
block_931:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_932
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_932
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_949
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_949
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_930
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_930
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_914
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_914
block_932:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_933
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_933
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_950
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_950
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_931
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_931
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_915
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_915
block_933:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_934
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_934
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_951
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_951
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_932
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_932
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_916
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_916
block_934:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_935
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_935
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_952
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_952
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_933
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_933
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_917
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_917
block_935:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_919
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_919
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_953
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_953
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_934
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_934
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_918
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_918
block_936:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_904
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_904
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_954
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_954
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_919
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_919
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_920
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_920
block_937:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_905
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_905
block_938:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_939
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_939
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_921
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_921
block_939:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_511
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_511
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_938
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_938
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_922
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_922
block_940:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_941
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_941
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_512
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_923
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_923
block_941:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_942
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_942
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_940
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_940
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_894
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_894
block_942:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_943
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_943
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_941
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_941
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_924
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_924
block_943:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_944
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_944
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_942
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_942
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_925
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_925
block_944:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_896
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_896
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_943
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_943
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_926
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_926
block_945:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_946
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_946
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_896
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_896
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_927
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_927
block_946:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_947
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_947
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_945
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_945
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_928
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_928
block_947:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_948
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_948
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_946
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_946
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_929
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_929
block_948:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_949
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_949
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_947
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_947
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_930
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_930
block_949:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_950
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_950
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_948
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_948
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_931
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_931
block_950:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_951
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_951
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_949
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_949
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_932
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_932
block_951:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_952
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_952
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_950
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_950
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_933
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_715
block_952:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_953
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_953
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_951
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_951
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_934
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_934
block_953:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_919
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_919
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_952
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_952
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_935
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_935
block_954:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_955
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_955
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_919
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_919
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_936
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_936
block_955:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_954
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_954
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_904
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_904
