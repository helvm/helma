; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_1
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_0
block_2:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_1
block_3:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_4
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_2
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_3
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_6
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_4
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_7
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_5
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_6
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_9
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_7
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_9
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_10
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_11
block_13:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_12
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 28
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_18
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_14
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_15
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_16
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 20
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_14
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_18
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_19
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_20
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 12
    jump block_23
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_24
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_22
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_25
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_23
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_24
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 24
    jump block_27
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_26
block_43:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_43
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_48
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_44
block_48:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_43
block_49:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_44
block_50:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_52
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_72
block_52:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 12
    jump block_53
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 12
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_50
block_53:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_54
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_52
block_54:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_55
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_53
block_55:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_56
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_54
block_56:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 16
    jump block_57
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 16
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_55
block_57:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_58
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_56
block_58:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_59
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_57
block_59:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_60
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_58
block_60:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 16
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 16
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_59
block_61:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_62
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_60
block_62:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_63
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_61
block_63:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_64
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_62
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_65
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_63
block_65:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_66
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_64
block_66:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_67
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_67
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_65
block_67:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 12
    jump block_68
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 12
    jump block_68
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_67
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_67
block_68:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_69
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_67
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_67
block_69:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_70
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_68
block_70:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_80
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_80
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_69
block_71:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_48
block_72:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_50
block_77:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_71
block_78:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_72
block_80:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 16
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_70
block_81:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    jump block_122
block_82:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_77
block_98:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_78
block_120:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_139
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_98
block_121:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_156
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_156
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_80
block_122:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    jump block_157
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_81
block_139:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_164
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_164
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_120
block_156:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_166
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_166
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_121
block_157:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_168
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_168
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_122
block_164:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_213
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 8
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 8
    jump block_139
block_165:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    jump block_214
block_166:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_165
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_165
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_165
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_156
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_156
block_167:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 16
    jump block_216
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_215
block_168:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_167
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_167
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_157
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_157
block_213:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_235
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_164
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_164
block_214:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    jump block_254
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_165
block_215:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 28
    jump block_350
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_167
block_216:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_255
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_255
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_167
block_235:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_264
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_264
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_213
block_254:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_265
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_265
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_214
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_214
block_255:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_266
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_216
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_216
block_264:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 20
    jump block_235
block_265:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_301
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_301
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_254
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_254
block_266:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_215
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_255
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_255
block_301:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_310
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_310
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_265
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_265
block_309:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_264
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_264
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_329
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_329
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_264
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_264
block_310:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_301
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_301
block_329:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_359
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_359
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_309
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_309
block_348:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_361
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_360
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_310
block_350:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_373
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_373
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_215
block_359:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_374
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_374
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 12
    jump block_329
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 12
    jump block_329
block_360:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_67
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_67
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_348
block_361:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    jump block_378
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    jump block_378
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_348
block_373:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_376
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_376
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_350
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_350
block_374:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_380
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 16
    jump block_359
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 16
    jump block_359
block_375:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 36
    jump block_385
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 36
    jump block_385
block_376:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_379
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_379
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_373
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_373
block_377:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_375
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_375
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_381
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_381
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_375
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_375
block_378:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    jump block_387
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 8
    jump block_387
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_361
block_379:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_383
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_383
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_376
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_376
block_380:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_409
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_409
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_374
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_374
block_381:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_386
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_386
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_377
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_377
block_382:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_406
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_406
block_383:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_388
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_388
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_379
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_379
block_385:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_375
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_375
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 20
    jump block_394
block_386:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_390
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_390
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_381
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_381
block_387:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_382
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_382
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_382
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_382
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_378
block_388:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_391
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_383
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_383
block_389:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_392
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_389
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_380
block_390:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_395
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_395
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_386
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_386
block_391:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_408
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_408
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_388
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_388
block_392:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_393
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_393
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_389
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_389
block_393:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_394
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_394
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_392
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_392
block_394:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_385
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_385
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_393
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_393
block_395:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_410
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_410
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_390
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_390
block_396:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    jump block_413
block_397:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_398
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_398
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_396
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_396
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_396
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_396
block_398:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_399
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_399
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_397
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_397
block_399:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_400
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_400
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_398
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_398
block_400:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_401
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_401
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_399
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_399
block_401:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_402
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_402
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_400
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_400
block_402:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_403
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_403
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_401
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_401
block_403:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_404
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_404
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    jump block_402
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    jump block_402
block_404:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_405
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_405
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_403
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_403
block_405:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_406
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_406
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_404
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_404
block_406:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 16
    jump block_405
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 16
    jump block_405
block_407:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 12
    jump block_415
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 12
    jump block_415
block_408:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_407
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_407
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_407
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_407
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_391
block_409:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_411
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_411
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_380
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_380
block_410:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_395
block_411:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_416
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_416
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_409
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_409
block_412:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 32
    jump block_410
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_422
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_422
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 32
    jump block_410
block_413:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    jump block_423
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_396
block_414:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_414
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_414
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_424
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_424
block_415:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    jump block_425
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_407
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_407
block_416:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_426
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_411
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_411
block_417:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_418
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_418
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_427
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_427
block_418:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_419
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_419
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_417
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_417
block_419:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_420
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_420
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_418
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_418
block_420:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_421
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_421
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_419
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_419
block_421:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_422
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_422
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_420
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_420
block_422:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_412
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_412
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_421
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_421
block_423:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_428
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_428
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_413
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_413
block_424:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_429
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_429
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_414
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_414
block_425:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_430
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_430
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_415
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_415
block_426:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_431
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_431
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_416
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_416
block_427:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_432
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 8
    jump block_417
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 8
    jump block_417
block_428:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_443
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_443
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_423
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_423
block_429:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 8
    jump block_424
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 8
    jump block_424
block_430:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_446
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_446
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_425
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_425
block_431:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_448
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_448
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 8
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 8
    jump block_426
block_432:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_433
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_433
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_427
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_427
block_433:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_434
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_434
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    jump block_432
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    jump block_432
block_434:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_435
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_435
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_433
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_433
block_435:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_436
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_436
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_434
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_434
block_436:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_437
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_437
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    jump block_435
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    jump block_435
block_437:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_438
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_438
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_436
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_436
block_438:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_439
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_439
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_437
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_437
block_439:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_440
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_440
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_438
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_438
block_440:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_441
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_441
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    jump block_439
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    jump block_439
block_441:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_442
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_442
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_440
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_440
block_442:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_443
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_443
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_441
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_441
block_443:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 16
    jump block_442
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_428
block_445:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_465
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_429
block_446:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_447
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_447
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_430
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_430
block_447:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_451
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_451
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_446
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_446
block_448:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_452
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_452
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_431
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_431
block_449:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_472
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 16
    jump block_471
block_450:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    jump block_474
block_451:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_450
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_450
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_447
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_447
block_452:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_453
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_453
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_448
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_448
block_453:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_454
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_454
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_452
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_452
block_454:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_455
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_455
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_453
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_453
block_455:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_456
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_456
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    jump block_454
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    jump block_454
block_456:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_457
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_457
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_455
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_455
block_457:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_458
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_458
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    jump block_456
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    jump block_456
block_458:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_459
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_459
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_457
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_457
block_459:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_460
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_460
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_458
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_458
block_460:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_461
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_461
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 16
    jump block_459
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 16
    jump block_459
block_461:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_462
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_462
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_460
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_460
block_462:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_463
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    jump block_461
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    jump block_461
block_463:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    jump block_462
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    jump block_462
block_465:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_466
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_466
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_445
block_466:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_467
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_467
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_465
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_465
block_467:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_468
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_468
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_466
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_466
block_468:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_469
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_469
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_467
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_467
block_469:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_470
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_470
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    jump block_468
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    jump block_468
block_470:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_471
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_471
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_469
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_469
block_471:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_449
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_449
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_470
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_470
block_472:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_473
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_473
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_449
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_449
block_473:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_474
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_474
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_472
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_472
block_474:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_450
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 8
    jump block_473
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    jump block_473
