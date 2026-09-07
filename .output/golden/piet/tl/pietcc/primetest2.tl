; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
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
    mod
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_0
block_2:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
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
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_2
block_5:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_3
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_9
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_9
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_10
block_13:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_12
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_13
block_16:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_14
block_22:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_16
block_23:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_22
block_24:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_23
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_23
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_8
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_8
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_27
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_25
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_26
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
block_29:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_5
block_31:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_24
block_32:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_25
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_25
block_33:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_31
block_34:
    switch_dpcc
block_35:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_32
block_36:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_46
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_33
block_37:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_34
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_41
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_54
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_54
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_40
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_53
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_35
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_46
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_43
block_46:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_82
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_36
block_52:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 96
    jump block_69
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 96
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_29
block_53:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_43
block_54:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_40
block_56:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_53
block_57:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_81
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_28
block_58:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_56
block_59:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_83
block_60:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_62
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_78
block_62:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_63
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_60
block_63:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_64
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_62
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_65
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 19
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 19
    jump block_63
block_65:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_66
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_64
block_66:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_67
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_67
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_65
block_67:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_68
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_66
block_68:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 9
    jump block_67
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 9
    jump block_67
block_69:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_68
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_52
block_71:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_81
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_57
block_72:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_62
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_62
block_73:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_71
block_74:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_82
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_46
block_75:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_76
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_59
block_76:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_75
block_77:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_78
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_76
block_78:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_60
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_77
block_79:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_80
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_80
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_73
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_46
block_80:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_81
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_46
block_81:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_46
block_82:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_82
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_82
block_83:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_59
block_84:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_54
block_86:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_89
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_58
block_87:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_82
block_88:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_83
block_89:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_86
block_90:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_105
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_89
block_91:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_116
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_87
block_96:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_88
block_98:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_96
block_99:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_109
block_100:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_37
block_102:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_103
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_100
block_103:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_141
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_141
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 101
    jump block_102
block_105:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_107
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_107
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_90
block_107:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_105
block_108:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_115
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_115
block_109:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_124
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_99
block_110:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_84
block_113:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_119
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_108
block_115:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_154
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_81
block_116:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_91
block_118:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_105
block_119:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_139
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_113
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_113
block_120:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_116
block_121:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_118
block_122:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_120
block_124:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_126
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_109
block_126:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_127
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_124
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_124
block_127:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_110
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_126
block_128:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_110
block_133:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_149
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_147
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_109
block_136:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_137
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_122
block_137:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_136
block_138:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_139
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_137
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_137
block_139:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_138
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_139
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_119
block_141:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_143
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_143
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_103
block_143:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_145
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_145
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_141
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_141
block_145:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_203
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 109
    jump block_143
block_147:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_148
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_155
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_133
block_148:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_147
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_124
block_149:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_150
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_148
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_124
block_150:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_151
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_149
block_151:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_152
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_152
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_150
block_152:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_153
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_151
block_153:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_153
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_152
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_152
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_110
block_154:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_115
block_155:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_173
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_173
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_147
block_158:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_172
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_172
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_154
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_154
block_159:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_160
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_164
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_164
block_160:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_162
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_162
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_159
block_162:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 105
    jump block_166
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_160
block_164:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_199
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 114
    jump block_159
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 114
    jump block_159
block_166:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_162
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_162
block_167:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_169
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_166
block_169:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_193
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_193
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_169
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_167
block_170:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_171
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_174
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_158
block_171:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_172
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_172
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_170
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_158
block_172:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_171
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_171
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_158
block_173:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_175
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_155
block_174:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_185
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_170
block_175:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_176
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_173
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_173
block_176:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 14
    jump block_177
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 14
    jump block_177
block_177:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_178
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_178
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_176
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_176
block_178:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_179
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_179
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_177
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_177
block_179:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_180
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_180
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_178
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_178
block_180:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 15
    jump block_181
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 15
    jump block_181
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_179
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_179
block_181:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_189
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_180
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_180
block_182:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 20
    jump block_183
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_183
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_190
block_183:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_184
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_184
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_182
block_184:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_185
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_183
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_183
block_185:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 112
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_184
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_174
block_189:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_190
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_190
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_181
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_181
block_190:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_182
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_189
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_189
block_193:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_200
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_200
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_169
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_169
block_194:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_195
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_185
block_195:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_196
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_195
block_196:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_197
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_195
block_197:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_198
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_199
block_198:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_199
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_199
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_199
block_199:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_164
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_164
block_200:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_201
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_201
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_169
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_193
block_201:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_202
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_200
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_200
block_202:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_203
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_203
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_201
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_201
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_203
block_203:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_145
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_145
