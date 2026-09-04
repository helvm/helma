; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_918
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_17:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_52
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_19
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_20
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_23
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_21
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_22
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_25
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_23
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_24
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_27
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_25
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_26
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_27
block_29:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_28
block_30:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_31
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_57
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_32
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_30
block_32:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_31
block_33:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_60
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_35
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_62
block_35:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_36
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_34
block_36:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_37
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_35
block_37:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    jump block_146
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 8
    jump block_146
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_39
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_126
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_38
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_41
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_39
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_42
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_78
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_17
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_17
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_41
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_42
block_44:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_43
block_45:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_17
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_17
block_46:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_48
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_92
block_48:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_49
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_46
block_49:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_50
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_48
block_50:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_51
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_49
block_51:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_95
block_52:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_19
block_55:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_25
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_28
block_56:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_29
block_57:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_30
block_59:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_32
block_60:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_33
block_61:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_33
block_62:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 45
    jump block_34
block_64:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_250
block_69:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_44
block_70:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_45
block_72:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_52
block_73:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_57
block_74:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_137
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_59
block_75:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_60
block_76:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_61
block_77:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_64
block_78:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_89
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_41
block_79:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_70
block_80:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_72
block_83:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_55
block_84:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_73
block_85:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_75
block_86:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_87
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_76
block_87:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_133
block_88:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_77
block_89:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_78
block_90:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_69
block_91:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_46
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_79
block_92:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_117
block_93:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_92
block_95:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_51
block_96:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_109
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_80
block_97:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_55
block_98:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_112
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_83
block_99:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_84
block_100:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_85
block_102:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_103
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_89
block_103:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_104
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_102
block_104:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 7
    jump block_90
block_106:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_93
block_108:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_95
block_109:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_96
block_110:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_119
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_97
block_111:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_98
block_112:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_120
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_98
block_114:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_100
block_115:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_125
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_88
block_117:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_118
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_153
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_106
block_118:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_179
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_117
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_108
block_119:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_134
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_110
block_120:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_135
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_112
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_112
block_121:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_99
block_124:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_145
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_145
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_62
block_125:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_147
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_115
block_126:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_149
block_127:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_129
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_103
block_129:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_151
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_104
block_130:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_189
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_189
block_131:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_163
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_163
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_152
block_133:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 25
    jump block_109
block_134:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_180
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_119
block_135:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_154
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_120
block_136:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_155
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_121
block_137:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_138
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_137
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_137
block_138:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_139
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_139
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_137
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_137
block_139:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_140
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_138
block_140:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_141
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_166
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_139
block_141:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_142
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_140
block_142:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_143
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_141
block_143:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_144
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_142
block_144:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_143
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_62
block_145:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_169
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_124
block_146:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_160
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_37
block_147:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_196
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_125
block_149:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_175
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_126
block_151:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_129
block_152:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_131
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_130
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_130
block_153:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_179
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_164
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_164
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_117
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_117
block_154:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_135
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_135
block_155:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_165
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_136
block_159:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_184
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_168
block_160:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_170
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_146
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_146
block_161:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_162
block_162:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_151
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_161
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_151
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_151
block_163:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_131
block_164:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_179
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_153
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_153
block_165:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 10
    jump block_155
block_166:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_167
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_140
block_167:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_168
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_168
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_166
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_166
block_168:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_159
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_159
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_167
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_167
block_169:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_185
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_145
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_145
block_170:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_160
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_160
block_171:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_172
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_172
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_186
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_186
block_172:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_173
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_186
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_171
block_173:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_174
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_172
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_172
block_174:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_149
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_173
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_173
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_149
block_175:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_176
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_176
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_149
block_176:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_161
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_175
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_161
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_161
block_178:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_190
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_163
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_163
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_163
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_163
block_179:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_191
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_164
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_118
block_180:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_192
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_192
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_134
block_181:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_194
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_111
block_182:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_165
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_165
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_165
block_184:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_185
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_159
block_185:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_184
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_169
block_186:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_172
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_172
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_171
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_171
block_188:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_207
block_189:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_130
block_190:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_202
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_178
block_191:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_203
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_179
block_192:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_194
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_180
block_194:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_324
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_324
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_192
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_192
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_181
block_195:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_182
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_133
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_133
block_196:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_247
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_247
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 5
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 5
    jump block_147
block_197:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_204
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_204
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_186
block_198:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_208
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_208
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_188
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_188
block_199:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_209
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_189
block_202:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_229
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 6
    jump block_190
block_203:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_213
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_191
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_191
block_204:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_205
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_197
block_205:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_206
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_204
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_204
block_206:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_205
block_207:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_188
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_216
block_208:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_218
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_218
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_188
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_198
block_209:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_210
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_219
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_219
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_199
block_210:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_211
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_228
block_211:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_220
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_211
block_212:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_221
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_221
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_202
block_213:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_222
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_222
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_203
block_214:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_215
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_206
block_215:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_216
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_216
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_214
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_214
block_216:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_207
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_215
block_218:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_227
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_227
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_208
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_208
block_219:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_210
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_209
block_220:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_228
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_228
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_211
block_221:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_229
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_229
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_212
block_222:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_230
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_213
block_224:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    jump block_225
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 6
    jump block_225
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_272
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_272
block_225:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_240
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_240
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_224
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_224
block_227:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_218
block_228:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_234
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_234
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_220
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_220
block_229:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_221
block_230:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_222
block_232:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 5
    jump block_227
block_234:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 8
    jump block_258
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_228
block_235:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_229
block_236:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_244
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_244
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_230
block_240:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_241
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_241
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_225
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_225
block_241:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_242
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_242
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_240
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_240
block_242:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_232
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_232
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_241
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_241
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_232
block_243:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_248
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_248
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_235
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_235
block_244:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_249
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_249
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_236
block_247:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_256
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_256
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_196
block_248:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_259
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_235
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_243
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_243
block_249:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_250
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_250
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_250
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_244
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_244
block_250:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 6
    jump block_260
block_256:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_271
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_271
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_247
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_247
block_258:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_273
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_273
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_234
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_234
block_259:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_260
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_260
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_248
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_248
block_260:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_250
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_250
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_259
block_261:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_262
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_262
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_309
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_309
block_262:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_263
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_309
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_261
block_263:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_262
block_264:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_265
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_275
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_263
block_265:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_275
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_275
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_264
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
block_266:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_267
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_267
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_266
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_266
block_267:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_344
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_344
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_266
block_270:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 25
    jump block_362
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 25
    jump block_362
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_270
block_271:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_282
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_282
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_256
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_256
block_272:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_283
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_283
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_224
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_224
block_273:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_292
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_292
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_258
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_258
block_274:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_261
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_261
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_293
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_293
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_261
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_261
block_275:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_276
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_276
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_310
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_310
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_264
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_265
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_265
block_276:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_277
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_277
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_275
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_275
block_277:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_297
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_297
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_276
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_276
block_281:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 24
    jump block_404
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 24
    jump block_404
block_282:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_329
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_329
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_271
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_271
block_283:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_284
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_284
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_272
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_272
block_284:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_285
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_285
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_283
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_283
block_285:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_286
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_286
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_284
block_286:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_287
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_285
block_287:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_288
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_288
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_286
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_286
block_288:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_289
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_289
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_287
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_287
block_289:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_290
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_290
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_288
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_288
block_290:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_291
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_291
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_289
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_289
block_291:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 5
    jump block_290
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 5
    jump block_290
block_292:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_302
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_302
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_273
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_273
block_293:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_261
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_261
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_309
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_309
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_274
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_274
block_297:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_323
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_323
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_277
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_277
block_298:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_312
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_312
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_426
block_299:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_300
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_300
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_452
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_452
block_300:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_314
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_314
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_299
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_299
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_270
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_270
block_302:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_316
block_303:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_304
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_304
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_317
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_317
block_304:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_305
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_305
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_303
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_303
block_305:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_306
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_306
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_320
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_320
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_304
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_304
block_306:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_307
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_307
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_320
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_320
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_305
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_305
block_307:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_308
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_308
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_320
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_320
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_306
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_306
block_308:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_320
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_320
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_307
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_307
block_309:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_262
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_262
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_321
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_293
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_261
block_310:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_322
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_322
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_275
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_275
block_312:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_327
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_298
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_298
block_314:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_328
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_328
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_300
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_300
block_316:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_335
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_334
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_302
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_302
block_317:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_336
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_336
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_303
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_303
block_320:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_352
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_390
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_336
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_304
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_305
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_308
block_321:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_338
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_338
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_309
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_309
block_322:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_341
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_341
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_310
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_310
block_323:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_342
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_342
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_297
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_297
block_324:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_343
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_343
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_194
block_325:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_326
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_357
block_326:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_327
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_327
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_325
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_325
block_327:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_312
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_312
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_326
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_326
block_328:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_345
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_314
block_329:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_347
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_347
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_282
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_282
block_330:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_331
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_331
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_350
block_331:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_332
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_332
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_330
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_330
block_332:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_333
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_333
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_331
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_331
block_333:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_291
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_291
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_332
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_332
block_334:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_335
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_335
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_335
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_335
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_316
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_316
block_335:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 7
    jump block_368
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_316
block_336:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_320
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_320
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_351
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_317
block_338:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_353
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_353
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_321
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_321
block_340:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_341
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_341
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_340
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_340
block_341:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_340
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_340
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_322
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_322
block_342:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_343
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_343
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_377
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_377
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_323
block_343:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_378
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_378
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_342
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_324
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_324
block_344:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_344
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_360
block_345:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_328
block_347:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_363
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_363
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_329
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_329
block_348:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_349
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_349
block_349:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_382
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_382
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_348
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_348
block_350:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_365
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_365
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_330
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_330
block_351:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_369
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_369
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_336
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_336
block_352:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_371
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_371
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_320
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_320
block_353:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_393
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_393
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_338
block_354:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_355
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_355
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_374
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_374
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_340
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_340
block_355:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_395
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_374
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_354
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_340
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_340
block_357:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_358
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_358
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_325
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_325
block_358:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_359
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_359
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_357
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_357
block_359:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_360
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_360
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_358
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_358
block_360:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_344
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_344
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_359
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_359
block_361:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_403
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_403
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_345
block_362:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_404
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_404
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_270
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_270
block_363:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_380
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_380
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_347
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_347
block_364:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_348
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_381
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_381
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_348
block_365:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_366
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_366
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_350
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_350
block_366:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_367
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_367
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_365
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_365
block_367:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_385
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_366
block_368:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_386
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_386
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_335
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_335
block_369:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_388
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_388
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_351
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_351
block_370:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_390
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_390
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_389
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_389
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_304
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_304
block_371:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_392
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_392
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_352
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_352
block_373:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_374
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_394
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_353
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_353
block_374:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_355
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_373
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_373
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_354
block_375:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_375
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_375
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_395
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_395
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_355
block_377:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_378
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_378
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_342
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_342
block_378:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_412
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_343
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_343
block_380:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_380
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_380
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_363
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_363
block_381:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_405
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_405
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_364
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_364
block_382:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_383
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_383
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_383
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_383
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_349
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_349
block_383:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_406
block_385:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_419
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_419
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_367
block_386:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_387
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_387
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_387
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_387
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_368
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_368
block_387:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_420
block_388:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_410
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_369
block_389:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_390
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_390
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_370
block_390:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_391
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_391
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_424
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_424
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_389
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_389
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_320
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_320
block_391:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_392
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_392
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_390
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_390
block_392:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_391
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_371
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_371
block_393:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_394
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_394
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_353
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_353
block_394:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_373
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_373
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_393
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_393
block_395:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_412
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_412
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_375
block_396:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_397
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_397
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_416
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_416
block_397:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_398
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_398
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_396
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_396
block_398:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_266
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_397
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_397
block_399:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 10
    jump block_451
block_400:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_401
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_434
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_434
block_401:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_402
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_402
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_400
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_400
block_402:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_403
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_403
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_401
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_401
block_403:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_361
block_404:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_404
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_281
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_458
block_405:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_418
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_418
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_381
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_381
block_406:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_407
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_407
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_383
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_383
block_407:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_419
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_419
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_406
block_409:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_385
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_385
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_385
block_410:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_388
block_412:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_378
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_378
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_442
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_442
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_395
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_395
block_416:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_432
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_432
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_396
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_396
block_418:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_437
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_437
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_405
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_405
block_419:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_409
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_409
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_407
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_407
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_385
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_409
block_420:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_438
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_438
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_387
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_387
block_422:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_423
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_439
block_423:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_410
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_410
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_422
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_422
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_410
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_410
block_424:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_440
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_390
block_426:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_427
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_427
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
block_427:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_428
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_426
block_428:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_429
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_429
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_427
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_427
block_429:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_442
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_442
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_428
block_431:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_412
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_412
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_378
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_378
block_432:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_444
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_416
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_416
block_433:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_452
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_399
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_399
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_399
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_399
block_434:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_453
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_453
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_400
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_400
block_436:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_460
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_281
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_281
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_281
block_437:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_380
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 5
    jump block_418
block_438:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_439
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_439
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_439
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_439
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_420
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_420
block_439:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_422
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_422
block_440:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_441
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_424
block_441:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_469
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_469
block_442:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_431
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_431
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_429
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_429
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_412
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_412
block_444:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_445
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_432
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_432
block_445:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_446
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_446
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_444
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_444
block_446:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_447
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_447
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_445
block_447:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_449
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_449
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_446
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_446
block_449:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_450
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_447
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_447
block_450:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_451
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_451
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_449
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_449
block_451:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_399
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_399
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_450
block_452:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_426
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_433
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_433
block_453:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_454
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_454
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_434
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_434
block_454:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_455
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_455
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_453
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_453
block_455:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_456
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_456
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_454
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_454
block_456:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_457
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_457
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_455
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_455
block_457:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_458
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_458
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_456
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_456
block_458:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_404
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_404
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_457
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_457
block_460:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_426
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_436
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_436
block_462:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_463
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_437
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_437
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_437
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_437
block_463:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_464
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_464
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_462
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_462
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_437
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_437
block_464:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_465
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_465
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_463
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_437
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_437
block_465:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_466
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_466
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_464
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_464
block_466:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 5
    jump block_465
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 5
    jump block_465
block_467:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_473
block_469:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 5
    jump block_477
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_467
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_441
block_471:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_489
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_489
block_472:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_473
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_473
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_471
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_471
block_473:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_467
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_467
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_472
block_475:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_482
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_481
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_469
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_469
block_477:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_469
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_482
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_482
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_482
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_483
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_469
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_469
block_481:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_475
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_475
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_467
block_482:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_469
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 6
    jump block_475
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 6
    jump block_475
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_469
block_483:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_477
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_477
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_482
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_482
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_482
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_482
block_489:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_491
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_491
block_491:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_492
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_492
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_489
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_489
block_492:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_493
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_493
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_491
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_491
block_493:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_494
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_494
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_492
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_492
block_494:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_495
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_495
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_493
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_493
block_495:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_496
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_564
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_564
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_494
block_496:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_497
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_495
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_495
block_497:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_498
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_498
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_496
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_496
block_498:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_509
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_509
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_497
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_497
block_500:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_515
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_515
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_426
block_504:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_536
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_536
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_426
block_506:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_541
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_541
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_466
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_466
block_509:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_542
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_542
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_498
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_498
block_510:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_512
block_512:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_513
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_513
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_510
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_510
block_513:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_514
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_514
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_512
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_512
block_514:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_515
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_515
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_513
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_513
block_515:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_517
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_517
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_515
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_515
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_514
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_514
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_500
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_500
block_517:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_547
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_547
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_515
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_515
block_518:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_519
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_583
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_583
block_519:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_520
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_520
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_518
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_518
block_520:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_521
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_521
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_521
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_521
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_519
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_519
block_521:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_522
block_522:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_523
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_521
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_521
block_523:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_524
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_524
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_522
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_522
block_524:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_525
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_525
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_523
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_523
block_525:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_526
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_526
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_524
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_524
block_526:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_527
block_527:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_528
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_528
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_549
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_549
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_526
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_526
block_528:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_549
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_549
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_527
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_527
block_530:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_531
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_531
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_873
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_873
block_531:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_533
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 22
    jump block_530
block_533:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_534
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_534
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_531
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_531
block_534:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_535
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_535
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_533
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_533
block_535:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 7
    jump block_534
block_536:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_537
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_537
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_552
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_504
block_537:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_554
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_554
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_536
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_536
block_538:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_539
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_539
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_555
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_555
block_539:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_540
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_540
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_538
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_538
block_540:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_557
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_557
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_539
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_539
block_541:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_559
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_559
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_506
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_506
block_542:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_564
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_564
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_509
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_509
block_543:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_510
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_510
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_565
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_565
block_547:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_566
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_566
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_517
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_517
block_549:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_584
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_584
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_526
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_526
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_527
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_528
block_551:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_567
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_567
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_535
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_535
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_535
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_535
block_552:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_568
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_568
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_536
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_536
block_554:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_569
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_569
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_537
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_537
block_555:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_570
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_570
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_538
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_538
block_557:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_558
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_558
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_558
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_558
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_540
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_540
block_558:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_571
block_559:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_574
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_574
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_541
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_541
block_560:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_591
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_591
block_561:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_562
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_560
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_560
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_560
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_560
block_562:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_563
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_563
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_561
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_561
block_563:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_576
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_591
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 8
    jump block_562
block_564:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_579
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_495
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_542
block_565:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_580
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_580
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_543
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_543
block_566:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_596
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_596
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_547
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_547
block_567:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_585
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_585
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_535
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_535
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_551
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_551
block_568:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_586
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_586
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_552
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_552
block_569:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_587
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_587
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_554
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_554
block_570:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_588
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_588
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_555
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_555
block_571:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_572
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_572
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_558
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_558
block_572:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_573
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_573
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_571
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_571
block_573:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_602
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_602
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_572
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_572
block_574:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_603
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_559
block_576:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_577
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_577
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_563
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_563
block_577:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_592
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_576
block_579:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_594
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_594
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_564
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_564
block_580:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_595
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_595
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_565
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_565
block_581:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_606
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_510
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_510
block_583:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_597
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_597
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_518
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_518
block_584:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_598
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_598
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_549
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_549
block_585:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_599
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_599
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_567
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_567
block_586:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_600
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_600
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_568
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_568
block_587:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_601
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_601
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_569
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_569
block_588:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 10
    jump block_570
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 10
    jump block_570
block_591:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_563
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_619
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_619
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_619
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_619
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_560
block_592:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_593
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_593
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_577
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_577
block_593:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_594
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_594
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_592
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_592
block_594:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_593
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_593
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_579
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_579
block_595:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_604
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_604
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_580
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_580
block_596:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_610
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_566
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_566
block_597:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_611
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_611
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_583
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_583
block_598:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 9
    jump block_621
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 9
    jump block_621
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_584
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_584
block_599:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_612
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_585
block_600:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_613
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_613
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_586
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_586
block_601:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_614
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_614
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_587
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_587
block_602:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_616
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_616
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_573
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_573
block_603:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_574
block_604:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_634
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_634
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_595
block_606:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_607
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_607
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_581
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_581
block_607:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_608
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_608
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_606
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_606
block_608:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_609
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_609
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_607
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_607
block_609:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_610
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_610
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_608
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_608
block_610:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_596
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_596
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_609
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_609
block_611:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_620
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_620
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_597
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_597
block_612:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 12
    jump block_599
block_613:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_623
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_623
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_600
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_600
block_614:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_689
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_689
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_601
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_601
block_615:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_588
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_588
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_641
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_641
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_588
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_588
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_588
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_588
block_616:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_626
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_626
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_602
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_602
block_617:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_618
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_618
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_627
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_627
block_618:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_603
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_603
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_617
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_617
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_603
block_619:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 9
    jump block_628
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
block_620:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_635
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_635
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_611
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_611
block_621:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_636
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_636
block_622:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_638
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_638
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_612
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_612
block_623:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_639
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_639
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_613
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_613
block_626:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_642
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_616
block_627:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_643
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_643
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_617
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_617
block_628:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_629
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_629
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_619
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_619
block_629:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_630
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_630
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_628
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_628
block_630:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_631
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_629
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_629
block_631:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_632
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_632
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_646
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_646
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_630
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_630
block_632:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_633
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_633
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_646
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_646
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_631
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_631
block_633:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_648
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_646
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_632
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_632
block_634:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 25
    jump block_604
block_635:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_650
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_650
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_620
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_620
block_636:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_637
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_637
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_621
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_621
block_637:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 16
    jump block_662
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 16
    jump block_662
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_636
block_638:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_651
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_651
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_612
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_622
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_622
block_639:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_652
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_652
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_623
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_623
block_640:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_653
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_665
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_665
block_641:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_615
block_642:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_655
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_655
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_626
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_626
block_643:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_668
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_655
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_655
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 9
    jump block_627
block_646:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_657
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_631
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_633
block_648:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_658
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_658
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_633
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_633
block_650:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_659
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_659
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_635
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_635
block_651:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_678
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_678
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_612
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_612
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_638
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_638
block_652:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_689
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_689
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_689
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_689
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 5
    jump block_639
block_653:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_654
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_654
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_640
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_640
block_654:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_641
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_641
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_653
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_653
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_641
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_641
block_655:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_643
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_643
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_666
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_666
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_642
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_642
block_656:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_683
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_683
block_657:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_674
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_673
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_646
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_646
block_658:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_675
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_675
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_648
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_648
block_659:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_676
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_676
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_650
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_650
block_660:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_661
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 7
    jump block_685
block_661:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 5
    jump block_677
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 5
    jump block_677
block_662:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    jump block_686
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_637
block_663:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_687
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_687
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_686
block_665:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_680
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_680
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_640
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_640
block_666:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_682
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_682
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_655
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_655
block_668:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 10
    jump block_643
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 10
    jump block_643
block_669:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_683
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_656
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4
    jump block_656
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_656
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
    jump block_669
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_669
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_669
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_669
block_671:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_672
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_672
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_669
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_669
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_670
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_670
block_672:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_673
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_673
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_671
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_671
block_673:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    jump block_672
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_657
block_674:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_673
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_673
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_673
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_673
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_657
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_657
block_675:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_684
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_684
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_658
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_658
block_676:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_685
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 7
    jump block_659
block_677:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_661
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_661
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_660
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_660
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_660
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_660
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_660
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_660
block_678:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_651
block_680:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_692
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 10
    jump block_665
block_682:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_668
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_668
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_668
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_668
block_683:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_669
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_669
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_690
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_690
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_656
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_656
block_684:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_708
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_708
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_675
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_675
block_685:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_660
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_660
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_676
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_676
block_686:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_663
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_663
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_662
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_662
block_687:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_688
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_688
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_663
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_663
block_688:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_678
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_678
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_687
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_687
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_678
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_678
block_689:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_689
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_689
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_691
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_691
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_652
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_652
block_690:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_693
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_693
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_683
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_683
block_691:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_704
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_704
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_689
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_689
block_692:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_706
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_706
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_680
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_680
block_693:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_694
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_694
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_694
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_694
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_690
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_690
block_694:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_695
block_695:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_696
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_696
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_694
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_694
block_696:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_697
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_695
block_697:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_698
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_698
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_696
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_696
block_698:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_699
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_699
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_697
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_697
block_699:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_714
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_699
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_698
block_704:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_705
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    jump block_705
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_738
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_691
block_705:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_706
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_706
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_704
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_704
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_738
block_706:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_707
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_707
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_735
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_735
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_705
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_705
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_692
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_692
block_707:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_708
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_708
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_706
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_706
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_738
block_708:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_708
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_708
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_707
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_707
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_738
block_709:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_724
block_710:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_711
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_711
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_709
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_709
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_709
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_709
block_711:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_712
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_712
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_710
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_710
block_712:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 5
    jump block_711
block_714:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_715
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_715
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_699
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_699
block_715:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_721
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_721
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_714
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_714
block_719:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_726
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_726
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_699
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_699
block_721:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_727
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_727
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_715
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_715
block_722:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_723
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_723
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
block_723:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_741
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_741
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_722
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_722
block_724:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_742
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_742
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_709
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_709
block_725:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_763
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_763
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_712
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_712
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_712
block_726:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_727
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_727
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_743
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_743
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_719
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_719
block_727:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_745
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_744
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_726
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_726
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_721
block_732:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_791
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_738
block_735:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_754
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_754
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_706
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_706
block_737:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_755
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_755
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_722
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_722
block_738:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_759
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_759
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_738
block_741:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_761
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_761
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_723
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_723
block_742:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_762
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_762
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_724
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_724
block_743:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_744
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_744
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_764
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_764
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_726
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_726
block_744:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_745
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_745
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_765
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_765
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_743
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_743
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_727
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_727
block_745:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 7
    jump block_826
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 7
    jump block_826
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_727
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_727
block_746:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_460
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_460
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_766
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_766
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_738
block_748:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_749
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_749
block_749:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    jump block_750
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_748
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_748
block_750:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_751
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_751
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_749
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_749
block_751:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_752
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_752
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_750
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_750
block_752:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_753
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_753
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_751
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_751
block_753:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_873
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_873
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_752
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_752
block_754:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_775
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_735
block_755:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_756
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_756
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_737
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_737
block_756:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_757
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_757
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_755
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_755
block_757:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_758
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_758
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_756
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_756
block_758:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 5
    jump block_757
block_759:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_799
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_799
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_738
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_738
block_760:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_918
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_800
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_760
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_760
block_761:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_778
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_778
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_741
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_741
block_762:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_779
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_742
block_763:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_781
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_781
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_725
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_725
block_764:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_765
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_782
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_805
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_805
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_743
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_743
block_765:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_745
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_745
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_782
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_764
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_764
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_744
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_744
block_766:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_827
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_827
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_746
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_746
block_769:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    jump block_788
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_787
block_770:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_789
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_788
block_771:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_792
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_792
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_748
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_748
block_775:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_793
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_793
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_754
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_754
block_776:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_798
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_798
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_758
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_758
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_758
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_758
block_778:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_801
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_801
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_761
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_761
block_779:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_802
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_802
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_762
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_762
block_781:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 4
    jump block_763
block_782:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_826
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_826
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_764
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_764
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_765
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_765
block_787:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_769
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_769
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_787
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_787
block_788:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_770
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_770
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_769
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_769
block_789:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_790
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_790
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_790
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_790
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_770
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_770
block_790:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_661
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_661
block_791:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_808
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_808
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_732
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_732
block_792:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_809
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_809
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_771
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_771
block_793:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_810
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_810
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_775
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_775
block_794:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_796
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_796
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_811
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_811
block_796:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_797
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_797
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_812
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_812
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_794
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_794
block_797:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_814
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_833
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_833
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_812
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_796
block_798:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_817
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_817
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_776
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_776
block_799:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_800
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_800
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_818
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_818
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_759
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_759
block_800:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_760
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_760
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_799
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_799
block_801:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_820
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_820
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_778
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_778
block_802:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_821
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_821
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_779
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_779
block_803:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_781
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_781
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_836
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_836
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_781
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_781
block_804:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_824
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_824
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_781
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_781
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_781
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_781
block_805:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_826
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_826
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_825
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_825
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_764
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_764
block_807:
    switch_dpcc
block_808:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_829
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_829
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_791
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_791
block_809:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_830
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_792
block_810:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_811
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_811
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_793
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_793
block_811:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_812
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_831
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_811
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_845
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_794
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_794
block_812:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_797
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_797
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_831
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_831
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_811
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_811
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_796
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_796
block_814:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_815
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_815
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_797
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_797
block_815:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_816
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_816
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_814
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_814
block_816:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_817
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_817
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_817
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_817
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_815
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_815
block_817:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_798
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_798
block_818:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 5
    jump block_888
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 5
    jump block_888
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_799
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_799
block_820:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_834
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_834
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_801
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_801
block_821:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_835
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_835
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_802
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_802
block_824:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_837
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_837
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_804
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_804
block_825:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_826
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_826
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_838
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_838
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_805
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_805
block_826:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_745
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_745
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_838
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_838
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_825
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_825
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_782
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_782
block_827:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_807
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_807
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_839
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_839
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_766
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_766
block_829:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_843
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_843
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_808
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_808
block_830:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_844
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_844
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_809
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_809
block_831:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_859
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_859
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_811
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_811
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_812
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_812
block_833:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_848
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_848
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_797
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_797
block_834:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_849
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_849
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_820
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_820
block_835:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_850
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_850
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_821
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_821
block_836:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_851
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_851
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_803
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_803
block_837:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_852
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_852
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_824
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_824
block_838:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_853
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_825
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_826
block_839:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_854
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_854
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_827
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_827
block_840:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_855
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_855
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_787
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_787
block_841:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    jump block_911
block_842:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 9
    jump block_881
block_843:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_842
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_842
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_842
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_842
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_829
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_829
block_844:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_844
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_844
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_830
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_830
block_845:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_858
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_858
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_811
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_811
block_848:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_860
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_860
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_833
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_833
block_849:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_760
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_760
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_834
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_834
block_850:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_863
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_863
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_835
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_835
block_851:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_864
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_864
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_836
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_836
block_852:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_890
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_890
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_837
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_837
block_853:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_865
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_865
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_838
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_838
block_854:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_867
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_867
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_839
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_839
block_855:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_868
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_868
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_840
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_840
block_857:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_873
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_873
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_871
block_858:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_874
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_874
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_845
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_845
block_859:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_875
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_875
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_831
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_831
block_860:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_861
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_861
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_848
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_848
block_861:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_862
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_862
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_860
block_862:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_861
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_861
block_863:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_877
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_877
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_850
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_850
block_864:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_878
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_878
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_851
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_851
block_865:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_879
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_879
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_879
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_879
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_853
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_853
block_867:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_880
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_880
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_854
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_854
block_868:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_922
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_921
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 9
    jump block_855
block_869:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_870
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_870
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_882
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_882
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_844
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_844
block_870:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_871
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_871
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_869
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_869
block_871:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_857
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_857
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_870
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_870
block_872:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_873
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_873
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_857
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_857
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_857
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_857
block_873:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_918
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_872
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_872
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_753
block_874:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_884
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_884
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_884
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_858
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_858
block_875:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_886
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_886
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_859
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_859
block_876:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_887
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_887
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_862
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_862
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_862
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_862
block_877:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_878
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_878
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_708
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_708
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_863
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_863
block_878:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_877
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_877
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_864
block_879:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_908
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_892
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_865
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_879
block_880:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_896
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_896
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_867
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_867
block_881:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_898
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_898
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_842
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_842
block_882:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_928
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_928
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_869
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_869
block_884:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_885
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_899
block_885:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_886
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_886
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_899
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_899
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_884
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_884
block_886:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_899
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_899
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_885
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_885
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_875
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_875
block_887:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_876
block_888:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_914
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_914
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_818
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_818
block_889:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_906
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_906
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_878
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_878
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_878
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_878
block_890:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_891
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_891
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_852
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_852
block_891:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_892
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_892
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_890
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_890
block_892:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_892
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_916
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_916
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_891
block_896:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_919
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_919
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_880
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_880
block_897:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4
    jump block_925
block_898:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_913
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_912
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_881
block_899:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_900
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_900
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_884
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_884
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_885
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_886
block_900:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_901
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_901
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_899
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_899
block_901:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_902
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_902
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_900
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_900
block_902:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_903
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_903
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_901
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_901
block_903:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_904
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_904
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_902
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_902
block_904:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_887
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_887
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_903
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_903
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_887
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_887
block_906:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_915
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_915
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_889
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_889
block_908:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_917
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_943
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_916
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_916
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_879
block_911:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_841
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_841
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_923
block_912:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_927
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_898
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_898
block_913:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_912
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_912
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_912
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_912
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_898
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_898
block_914:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_940
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_940
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_888
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_888
block_915:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_941
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_877
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_877
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 2
    jump block_906
block_916:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_908
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_943
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 6
    jump block_942
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_892
block_917:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_943
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_943
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_908
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_908
block_918:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_919
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_919
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
block_919:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_634
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_634
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_732
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_732
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_918
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_918
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_896
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_896
block_921:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_922
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_922
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_868
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_868
block_922:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_923
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_923
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_921
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_921
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_868
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_868
block_923:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_911
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_911
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_922
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_922
block_924:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_925
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_925
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_841
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_841
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_841
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_841
block_925:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_897
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_897
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_924
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_924
block_926:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_927
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_927
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_897
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_897
block_927:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_912
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_912
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_926
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_926
block_928:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_918
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_918
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_882
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_882
block_930:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_931
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_931
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_704
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_704
block_931:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_932
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_932
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_704
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_704
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_930
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_930
block_932:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_934
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_934
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_931
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_931
block_934:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_935
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_935
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_932
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_932
block_935:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_936
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_936
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_934
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_934
block_936:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_937
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_937
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_935
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_935
block_937:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_938
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_938
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_936
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_936
block_938:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_939
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_939
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_937
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_937
block_939:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_940
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_940
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_938
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_938
block_940:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_939
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_939
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_914
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_914
block_941:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_942
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_942
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_915
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_915
block_942:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_916
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_916
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_941
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_941
block_943:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_917
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_917
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_916
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_916
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_908
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_908
