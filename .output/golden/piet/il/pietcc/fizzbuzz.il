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
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_1
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
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
    push 11
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
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
    pop
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_5
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_6
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_9
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_7
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_9
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_10
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_11
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_12
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_14
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_15
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_16
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_19
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_17
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_18
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_19
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_21
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_24
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
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
    not
    jump block_25
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_23
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_24
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_68
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_25
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_26
block_29:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_28
block_30:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 10
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_29
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_32
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_30
block_32:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_34
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_31
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_35
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_32
block_35:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_36
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_34
block_36:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_37
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_35
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_38
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_36
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 11
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_37
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_38
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_39
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_42
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_40
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_41
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_42
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_45
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_43
block_45:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_47
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_47
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_44
block_47:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_60
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_45
block_60:
    switch_dpcc
block_61:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_66
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_12
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_12
block_66:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_68
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_68
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_19
block_68:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_70
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_26
block_70:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_72
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_34
block_72:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_72
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_45
block_82:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_111
block_84:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_86
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_64
block_86:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_66
block_88:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_68
block_90:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_82
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_70
block_101:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_127
block_104:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_106
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_86
block_106:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_88
block_108:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_101
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_125
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_125
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_90
block_111:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_111
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_129
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_82
block_121:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_144
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_104
block_125:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_127
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_140
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_108
block_127:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_129
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_129
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_125
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_101
block_129:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_129
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_129
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_111
block_140:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_125
block_142:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_144
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_144
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_368
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_368
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_142
block_144:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_146
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_146
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_168
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_168
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_121
block_146:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_140
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_140
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_348
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_144
block_149:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_151
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_151
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_171
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_171
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_140
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_127
block_151:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_153
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_415
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_415
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_149
block_153:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_153
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_174
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_151
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_129
block_168:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_175
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_144
block_171:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_149
block_174:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_177
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_177
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_153
block_175:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_168
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_168
block_176:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_171
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_171
block_177:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_174
block_178:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_181
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_175
block_179:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_182
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_182
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_176
block_180:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_183
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_177
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_177
block_181:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_184
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_178
block_182:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_185
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_179
block_183:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_186
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_180
block_184:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_187
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_187
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_181
block_185:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_182
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_182
block_186:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_189
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_183
block_187:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_190
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_184
block_188:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_191
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_191
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_185
block_189:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_192
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_192
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_186
block_190:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_193
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_187
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_187
block_191:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_188
block_192:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_189
block_193:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_196
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_190
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_190
block_194:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_191
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_191
block_195:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_198
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_192
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_192
block_196:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_199
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_193
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_193
block_197:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_200
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_200
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_194
block_198:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_201
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_201
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_195
block_199:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_202
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_196
block_200:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_203
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_197
block_201:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_204
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_204
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_198
block_202:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 9
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 9
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_199
block_203:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    jump block_206
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 10
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_200
block_204:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    jump block_207
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 10
    jump block_207
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_201
block_205:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_208
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_202
block_206:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_203
block_207:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_204
block_208:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_211
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_205
block_209:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_212
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_206
block_210:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_213
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_207
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_207
block_211:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_214
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_208
block_212:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 7
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_209
block_213:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 7
    jump block_216
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_210
block_214:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_217
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_217
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_211
block_215:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_218
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_218
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_212
block_216:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_219
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_219
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_213
block_217:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_220
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_214
block_218:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_221
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_215
block_219:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_222
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_222
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_216
block_220:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_223
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_217
block_221:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_224
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_218
block_222:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_225
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_219
block_223:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_226
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_220
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_220
block_224:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_227
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_227
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_221
block_225:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_228
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_228
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_222
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_222
block_226:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_223
block_227:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_224
block_228:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_231
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_231
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_225
block_229:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_232
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_226
block_230:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_233
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_233
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_227
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_227
block_231:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_234
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_228
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_228
block_232:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_229
block_233:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_246
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_246
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_230
block_234:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_240
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_240
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_231
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_231
block_235:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_241
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_241
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_232
block_236:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_238
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_238
block_238:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_246
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_246
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_259
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_236
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_238
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_238
block_240:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_247
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_247
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_234
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_234
block_241:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_258
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_258
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_235
block_246:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_252
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_252
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_233
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_233
block_247:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_253
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_240
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_240
block_248:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_250
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_250
block_250:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_258
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_258
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_273
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_273
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_248
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_248
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_250
block_252:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_266
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_266
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_246
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_246
block_253:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_267
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_267
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_247
block_258:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_268
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_268
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_241
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_241
block_259:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_260
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_260
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_309
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_309
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_238
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_238
block_260:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_262
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_262
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_259
block_262:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_263
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_260
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_260
block_263:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_265
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_265
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_262
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_262
block_265:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_266
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_263
block_266:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_281
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_281
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_265
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_265
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_252
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_252
block_267:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_272
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_253
block_268:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_280
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_280
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_258
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_258
block_272:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_282
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_282
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_267
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_267
block_273:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_274
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_274
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_321
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_321
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_250
block_274:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_276
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_276
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_273
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_273
block_276:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_277
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_277
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_274
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_274
block_277:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_276
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_276
block_279:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_280
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_280
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_277
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_277
block_280:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_288
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_288
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_279
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_279
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_268
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_268
block_281:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_286
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_266
block_282:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_287
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_287
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_272
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_272
block_286:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_289
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_289
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_281
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_281
block_287:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_290
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_290
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_282
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_282
block_288:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_291
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_280
block_289:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_292
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_292
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_286
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_286
block_290:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 9
    jump block_293
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 9
    jump block_293
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_287
block_291:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_294
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_294
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_288
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_288
block_292:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_295
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_295
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_289
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_289
block_293:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_296
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_290
block_294:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_297
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_297
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_291
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_291
block_295:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_298
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_298
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_292
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_292
block_296:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_299
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_299
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_293
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_293
block_297:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_300
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_300
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_294
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_294
block_298:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_301
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_301
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_295
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_295
block_299:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_302
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_296
block_300:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_303
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_303
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_297
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_297
block_301:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_304
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_304
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_298
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_298
block_302:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_305
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_305
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_299
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_299
block_303:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_306
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_306
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_300
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_300
block_304:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_312
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_312
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_301
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_301
block_305:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 6
    jump block_313
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 6
    jump block_313
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_302
block_306:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_314
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_314
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_303
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_303
block_307:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_309
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_309
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_350
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_350
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_236
block_309:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_311
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_311
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_325
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_325
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_307
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_307
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_259
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_259
block_311:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_312
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_312
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_309
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_309
block_312:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_311
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_311
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_304
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_304
block_313:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_318
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_305
block_314:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_324
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_324
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_306
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_306
block_318:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_327
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_327
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_313
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_313
block_319:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_321
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_321
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_370
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_248
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_248
block_321:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_323
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_323
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_335
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_335
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_319
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_273
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_273
block_323:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_324
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_324
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_321
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_321
block_324:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_323
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_323
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_314
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_314
block_325:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_333
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_333
block_327:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    jump block_334
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_318
block_333:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    jump block_337
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_333
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_333
block_334:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_338
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_338
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_327
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_327
block_335:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_341
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_341
block_337:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_342
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_342
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_333
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_333
block_338:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 8
    jump block_343
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_334
block_341:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    jump block_344
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_341
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_341
block_342:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_345
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_345
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_337
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_337
block_343:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_346
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_346
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_338
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_338
block_344:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_347
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_347
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_341
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_341
block_345:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_354
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_354
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_342
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_342
block_346:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_367
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_367
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_343
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_343
block_347:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_359
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_359
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_344
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_344
block_348:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_350
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_350
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_146
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_146
block_350:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_352
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_352
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_350
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_350
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_348
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_307
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_307
block_352:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_354
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_354
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_352
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_352
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_350
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_350
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_325
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_325
block_354:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_352
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_352
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_345
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_345
block_355:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_357
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_357
block_357:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_367
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_367
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_379
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_379
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_357
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_357
block_359:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_374
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_374
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_347
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_347
block_367:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_375
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_346
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_346
block_368:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_370
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_370
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_368
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_368
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_142
block_370:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_372
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_372
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_370
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_368
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_368
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_319
block_372:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_374
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_374
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_372
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_372
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_335
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_335
block_374:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_372
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_372
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_359
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_359
block_375:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_386
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_386
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_367
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_367
block_379:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_380
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_380
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_400
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_400
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_357
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_357
block_380:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_382
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_382
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_379
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_379
block_382:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_383
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_383
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_380
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_380
block_383:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_385
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_385
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_382
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_382
block_385:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_386
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_386
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_383
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_383
block_386:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_390
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_390
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_385
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_385
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_375
block_390:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_386
block_391:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_392
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_392
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_390
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_390
block_392:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_393
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_393
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_391
block_393:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_394
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_394
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_392
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_392
block_394:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_395
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_395
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_393
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_393
block_395:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_396
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_396
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_394
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_394
block_396:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_397
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_397
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_395
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_395
block_397:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_403
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_403
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_396
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_396
block_398:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_400
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_400
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_417
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_417
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_355
block_400:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_402
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_402
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_407
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_407
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_398
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_398
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_379
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_379
block_402:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_403
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_403
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_400
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_400
block_403:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_402
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_402
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_397
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_397
block_407:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_411
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_411
block_411:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 10
    jump block_412
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_411
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_411
block_412:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_413
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_413
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_411
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_411
block_413:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_414
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_414
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_412
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_412
block_414:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_421
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_421
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_413
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_413
block_415:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_417
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_417
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_415
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_415
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_151
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_151
block_417:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_419
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_419
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_417
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_417
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_415
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_415
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_398
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_398
block_419:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_421
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_421
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_419
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_419
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_417
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_417
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_407
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_407
block_421:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_419
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_419
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_414
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_414
