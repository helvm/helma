; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_1
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_1
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_0
block_3:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_4
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_4
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_1
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_3
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_6
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_6
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_4
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_7
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_7
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_5
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_6
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_9
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_9
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_7
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_1
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_9
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_12
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_10
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_85
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_11
block_13:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_12
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_13
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_14
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_15
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 14
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 14
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_10
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_26
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_24
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_20
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_23
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_20
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_38
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_26
block_29:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_19
block_30:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_29
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_30
block_35:
    switch_dpcc
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 10
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_33
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_35
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_28
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_38
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_37
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_39
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 25
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 25
    jump block_42
block_46:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_47
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_47
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_41
block_47:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_46
block_48:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_47
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_47
block_50:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 15
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_48
block_53:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_44
block_55:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_50
block_56:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_53
block_58:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_55
block_59:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_56
block_60:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_58
block_61:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_59
block_62:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_60
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_62
block_65:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_64
block_66:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_61
block_69:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_70
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_69
block_70:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_85
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_65
block_73:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_66
block_75:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_78
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_70
block_77:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 10
    jump block_73
block_78:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_75
block_79:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_78
block_81:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_79
block_84:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_81
block_85:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_12
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_12
block_86:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_84
block_88:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_85
block_89:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 11
    jump block_90
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 11
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_90:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_91
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_91
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_16
block_91:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_92
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_92:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 17
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 17
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_93:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 10
    jump block_94
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 10
    jump block_94
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_92
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_94:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 10
    jump block_95
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_95:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_96
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_94
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_96:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_97
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_97
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_95
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_97:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_98
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_98
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_98:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 15
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_99:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_98
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_100:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_101
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_101:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_102
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_102:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_103
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_103:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_104:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_105
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_105:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_106
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_106:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_107
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_107:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_108:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_77
block_110:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_86
