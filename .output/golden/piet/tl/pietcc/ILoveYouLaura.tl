; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 73
    jump block_14
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 117
    jump block_6
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_21
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_5
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 97
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_6
block_14:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_0
block_15:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_14
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_8
block_21:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_5
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_28
block_28:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_25
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_25
block_29:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_29
block_30:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 76
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_30
block_32:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_29
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_15
block_35:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_33
block_36:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_28
block_37:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 32
    jump block_49
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 97
    jump block_52
block_43:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_30
block_45:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_97
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_43
block_49:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_37
block_51:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_38
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_51
block_52:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_54
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_38
block_54:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 114
    jump block_55
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 114
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_52
block_55:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 7
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_54
block_56:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_56
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_36
block_58:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_59
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_55
block_59:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_59
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_21
block_60:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_56
block_63:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_72
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_65
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_51
block_65:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_65
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_49
block_68:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_69
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_68
block_69:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 101
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_68
block_71:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_89
block_72:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_72
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_95
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_63
block_74:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_32
block_75:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_80
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_60
block_76:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_74
block_80:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_83
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_75
block_81:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_109
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_80
block_83:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_76
block_84:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 32
    jump block_86
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 32
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_71
block_86:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 7
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_84
block_88:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_89
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_69
block_89:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_71
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_88
block_92:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_86
block_93:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_81
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_92
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_81
block_94:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_68
block_95:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_72
block_96:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_63
block_97:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_45
block_98:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 118
    jump block_94
block_102:
    switch_dpcc
block_106:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 32
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 32
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_97
block_109:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_119
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_81
block_110:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_106
block_114:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_140
block_115:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_115
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_95
block_119:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_109
block_122:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_110
block_126:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_127
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_115
block_127:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_128
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_126
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_127
block_128:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_129
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_127
block_129:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_119
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_119
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_129
block_130:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_131
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_114
block_131:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_141
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 111
    jump block_130
block_134:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_142
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 117
    jump block_141
block_136:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_98
block_138:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_140
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 89
    jump block_136
block_140:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_114
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_138
block_141:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_134
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_131
block_142:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_122
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_134
