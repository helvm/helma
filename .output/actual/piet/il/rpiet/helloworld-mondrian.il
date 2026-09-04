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
    not
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
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
    push 10
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 10
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_1
block_3:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_2
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_6
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_3
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_5
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_6
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_12
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_11
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_40
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_14
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_42
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_17
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 14
    jump block_39
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 14
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_43
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_41
block_36:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_37
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_8
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_38
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_36
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_37
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_20
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_12
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_15
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_33
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_18
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_45
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_39
block_45:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_46
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_44
block_46:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_45
block_50:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_46
block_51:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_2
block_62:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_50
block_63:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_69
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_69
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_65
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_63
block_65:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_66
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_3
block_66:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_62
block_69:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_63
block_70:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_71
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_69
block_71:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_92
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_70
block_72:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 11
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 11
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_92
block_74:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_75
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_109
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_74
block_75:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_94
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_109
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_74
block_76:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 19
    jump block_95
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_94
block_85:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_86
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_95
block_86:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_85
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_85
block_87:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_88
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_112
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_96
block_88:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_97
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_112
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_87
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_87
block_89:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 12
    jump block_90
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 12
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_97
block_90:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_89
block_92:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_72
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_107
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_71
block_93:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_74
block_94:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_76
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_109
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_75
block_95:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_85
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_76
block_96:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_87
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_87
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_86
block_97:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_89
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_89
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_88
block_98:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_99
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_90
block_99:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_100
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_98
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_98
block_100:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_99
block_101:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_105
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_100
block_102:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_103
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_70
block_103:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_104
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_71
block_104:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_105
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_105
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_107
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_71
block_105:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_101
block_107:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_104
block_108:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_109
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_72
block_109:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_110
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_75
block_110:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_76
block_111:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_112
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_86
block_112:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_87
block_113:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_112
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_90
block_114:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_102
block_122:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_123
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_123
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_114
block_123:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_134
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_122
block_124:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 11
    jump block_135
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 11
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_134
block_126:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_127
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_135
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_126
block_127:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_126
block_128:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_129
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_129
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_137
block_129:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_148
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_128
block_130:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_131
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_131
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_156
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_156
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_139
block_131:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    jump block_140
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_156
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_130
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_130
block_132:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_136
block_133:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_139
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_150
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_138
block_134:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_124
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_124
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_123
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_123
block_135:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_126
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_124
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_126
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_126
block_136:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_132
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_132
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_127
block_137:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_128
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_132
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_132
block_138:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_133
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_129
block_139:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_130
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_130
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_150
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_133
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_133
block_140:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_141
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_141
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_131
block_141:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_142
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_142
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_140
block_142:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_143
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_143
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_159
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_141
block_143:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_144
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_144
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_142
block_144:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_145
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_145
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_143
block_145:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_147
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_147
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_144
block_147:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_151
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_145
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_145
block_148:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_170
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_170
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_129
block_149:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_150
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_148
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_138
block_150:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_151
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_151
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_155
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_139
block_151:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_150
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_147
block_155:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_156
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_156
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_150
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_150
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_150
block_156:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_157
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 5
    jump block_155
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 5
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_130
block_157:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_158
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_156
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_131
block_158:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    jump block_157
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    jump block_157
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_140
block_159:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_160
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_142
block_160:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 20
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 20
    jump block_159
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_143
block_161:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    jump block_162
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 6
    jump block_162
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_201
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_201
block_162:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_163
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_202
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_161
block_163:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_165
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_203
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_203
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_162
block_165:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    jump block_166
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 6
    jump block_166
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_163
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_165
block_166:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_165
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_165
block_167:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_168
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_206
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_166
block_168:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    jump block_169
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 6
    jump block_169
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_207
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_167
block_169:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_208
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_208
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_168
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_168
block_170:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_148
block_171:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_172
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_178
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_187
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_187
block_172:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_174
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_174
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_178
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_178
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_171
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_171
block_174:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_175
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_182
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_181
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_172
block_175:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_184
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_174
block_176:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_177
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_177
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_209
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_209
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_170
block_177:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_176
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_176
block_178:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_179
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_194
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_187
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_171
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_172
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_172
block_179:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_180
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_180
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_195
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_178
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_174
block_180:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_181
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_181
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_196
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_179
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_174
block_181:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_174
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_174
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_180
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_174
block_182:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_183
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_183
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_175
block_183:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_184
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_198
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_182
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_175
block_184:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_183
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_175
block_185:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_184
block_186:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_191
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_212
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_190
block_187:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_178
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_178
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_193
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_193
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_171
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_171
block_188:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_189
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_189
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_177
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_177
block_189:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_190
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_212
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_211
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_188
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_188
block_190:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_186
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_186
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_212
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_212
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_189
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_189
block_191:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_192
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_192
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_214
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_186
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_186
block_192:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_193
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_193
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_214
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_191
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_191
block_193:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_194
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_192
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_192
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_187
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_187
block_194:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_195
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_193
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_193
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_178
block_195:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_196
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_179
block_196:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_197
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_200
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_200
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_180
block_197:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_198
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_182
block_198:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_185
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_183
block_200:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_217
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_217
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_196
block_201:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_202
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_161
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_161
block_202:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_203
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_201
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_201
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_162
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_162
block_203:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_204
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_163
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_163
block_204:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_205
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_165
block_205:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_204
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_204
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_166
block_206:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_167
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_167
block_207:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_208
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_208
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_168
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_168
block_208:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_209
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_207
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_169
block_209:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_210
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_208
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_176
block_210:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_211
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_209
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_177
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_177
block_211:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_212
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_212
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_189
block_212:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_213
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_211
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_190
block_213:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_214
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_214
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_186
block_214:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_213
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_191
block_215:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_216
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_216
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_214
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_193
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_193
block_216:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_193
block_217:
    switch_dpcc
