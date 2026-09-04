; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 64
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 64
    jump block_1
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 25
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
    push 7
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 7
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_1
block_3:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_4
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_2
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_3
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_6
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_6
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_4
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_7
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_7
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_5
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_6
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_9
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_9
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_7
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 64
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_172
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_9
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_12
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_10
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_11
block_13:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 7
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 7
    jump block_97
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 7
    jump block_97
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_12
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_13
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_14
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_15
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_38
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_16
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_19
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_19
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_17
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_18
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_19
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_20
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_23
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_21
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_24
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
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
    push 1
    jump block_25
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_23
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_24
block_26:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_47
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_25
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_2
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_5
block_29:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_6
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_6
block_30:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_31
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_7
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_7
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_32
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_8
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_8
block_32:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_9
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_9
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_34
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_53
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_11
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_54
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_12
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_12
block_35:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_36
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_14
block_36:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_37
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_15
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_38
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_38
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_16
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_39
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_58
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_17
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_17
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_18
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_41
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_19
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_42
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_20
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_21
block_43:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_22
block_45:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_24
block_47:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_26
block_48:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_27
block_49:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_50
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_29
block_50:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_51
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_30
block_51:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_52
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_73
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_31
block_52:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_32
block_53:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_54
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_54
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_33
block_54:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_34
block_55:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_56
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_78
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_35
block_56:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_57
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_36
block_57:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_58
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_58
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_80
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_80
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_37
block_58:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_59
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_38
block_59:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_60
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_82
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_39
block_60:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_83
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_40
block_61:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_62
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_41
block_62:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_85
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_42
block_64:
    switch_dpcc
block_66:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_1
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_2
block_68:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_69
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_4
block_69:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_71
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_89
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_4
block_71:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_72
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_91
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_49
block_72:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_73
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_92
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_50
block_73:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_51
block_74:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_94
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_94
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_73
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_52
block_75:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_76
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_95
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_95
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_53
block_76:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_54
block_78:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_55
block_79:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_80
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_80
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_56
block_80:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_81
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_57
block_81:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_82
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_80
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_58
block_82:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_83
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_103
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_59
block_83:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_84
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_82
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_60
block_84:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_85
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_105
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_83
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_61
block_85:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_62
block_89:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_90
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_69
block_90:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_91
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_28
block_91:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_92
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_92
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_116
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_71
block_92:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_91
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_72
block_93:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_94
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_94
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_92
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_73
block_94:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_119
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_119
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_74
block_95:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_96
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_120
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_75
block_96:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_97
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_97
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_95
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_76
block_97:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_98
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_13
block_98:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_99
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_123
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_123
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_97
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_97
block_99:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_100
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_124
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_124
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_98
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_78
block_100:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_101
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_125
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_125
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_79
block_101:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_102
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_80
block_102:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_103
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_103
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_81
block_103:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_104
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_82
block_104:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_105
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_83
block_105:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_106
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_84
block_106:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_107
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_85
block_107:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_106
block_108:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_107
block_114:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_115
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_130
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_89
block_115:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_116
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_116
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_131
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_90
block_116:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_117
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_132
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_132
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_91
block_117:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_118
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_133
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_92
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_92
block_118:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_119
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_119
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_134
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_117
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_93
block_119:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_135
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_94
block_120:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_121
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_136
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_95
block_121:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_122
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_137
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_120
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_96
block_122:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_123
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_123
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_97
block_123:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_124
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_124
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_139
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_139
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_98
block_124:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_125
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_125
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_140
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_123
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_123
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_99
block_125:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_126
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_141
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_124
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_100
block_126:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_127
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_142
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_125
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_101
block_127:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_128
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_143
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_126
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_102
block_128:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_144
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_103
block_130:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_131
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_131
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_148
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_114
block_131:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_132
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_132
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_130
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_115
block_132:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_133
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_150
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_116
block_133:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_134
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_134
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_151
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_132
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_132
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_117
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_117
block_134:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_135
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_152
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_152
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_133
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_118
block_135:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_119
block_136:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_137
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_154
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_120
block_137:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_138
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_155
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_155
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_136
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_121
block_138:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_139
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_139
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_156
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_156
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_137
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_137
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_122
block_139:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_140
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_140
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_157
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_157
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_123
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_123
block_140:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_141
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_141
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_139
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_124
block_141:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_142
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_142
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_159
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_140
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_125
block_142:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_143
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_143
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_141
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_126
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_126
block_143:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_144
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_144
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_161
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_127
block_144:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_162
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_162
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_143
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_128
block_148:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_149
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_166
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_166
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_130
block_149:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_150
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_150
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_167
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_148
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_131
block_150:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_151
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_151
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_168
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_168
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_132
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_132
block_151:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_152
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_152
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_169
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_169
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_150
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_133
block_152:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_153
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_170
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_170
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_151
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_134
block_153:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_171
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_171
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_152
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_152
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_135
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_135
block_154:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_155
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_155
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_180
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_180
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_136
block_155:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_156
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_156
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_181
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_181
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_154
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_154
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_137
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_137
block_156:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_157
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_157
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_182
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_155
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_138
block_157:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_158
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_183
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_183
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_156
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_156
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_139
block_158:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_159
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_159
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_184
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_184
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_157
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_157
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_140
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_140
block_159:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_160
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_141
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_141
block_160:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_161
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_161
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_186
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_186
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_159
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_142
block_161:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_162
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_162
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_187
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_187
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_160
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_143
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_143
block_162:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_163
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_163
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_188
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_161
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_144
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_144
block_163:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_189
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_189
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_162
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_162
block_166:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_167
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_194
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_148
block_167:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_168
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_168
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_195
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_166
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_166
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_149
block_168:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_169
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_169
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_196
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_167
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_150
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_150
block_169:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_170
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_170
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_168
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_168
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_151
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_151
block_170:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_171
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_171
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_198
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_169
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_152
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_152
block_171:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_172
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_172
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_199
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_199
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_170
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_153
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_153
block_172:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_173
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_173
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_200
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_200
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_171
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_171
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_10
block_173:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_174
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_174
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_201
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_201
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_172
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_172
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_10
block_174:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_175
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_175
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_202
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_173
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_173
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_10
block_175:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_176
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_203
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_203
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_10
block_176:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_177
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_177
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_204
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_204
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_175
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_10
block_177:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_178
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_178
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_176
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_10
block_178:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_179
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_206
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_177
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_177
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_10
block_179:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_180
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_180
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_207
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_178
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_10
block_180:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_181
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_181
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_208
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_208
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_179
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_154
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_154
block_181:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_182
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_182
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_209
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_209
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_180
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_155
block_182:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_183
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_183
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_181
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_156
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_156
block_183:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_184
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_184
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_211
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_211
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_182
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_157
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_157
block_184:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_185
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_212
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_212
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_183
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_158
block_185:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_186
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_186
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_213
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_213
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_184
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_159
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_159
block_186:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_187
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_187
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_214
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_185
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_160
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_160
block_187:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_188
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_215
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_186
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_161
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_161
block_188:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_189
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_189
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_216
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_216
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_187
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_187
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_162
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_162
block_189:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_188
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_163
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_163
block_194:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_195
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_195
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_221
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_166
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_166
block_195:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_196
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_196
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_222
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_222
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_167
block_196:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_197
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_223
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_223
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_168
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_168
block_197:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_198
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_198
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_224
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_224
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_169
block_198:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_199
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_199
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_225
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_225
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_170
block_199:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_200
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_200
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_226
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_226
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_171
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_171
block_200:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_201
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_201
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_227
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_227
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_199
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_172
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_172
block_201:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_202
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_202
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_228
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_228
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_200
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_200
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_173
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_173
block_202:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_203
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_203
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_229
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_229
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_201
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_201
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_174
block_203:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_204
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_204
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_230
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_230
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_175
block_204:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_205
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_231
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_231
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_203
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_176
block_205:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_206
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_206
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_232
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_232
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_204
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_204
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_177
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_177
block_206:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_207
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_207
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_233
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_233
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_178
block_207:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_208
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_208
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_234
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_234
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_179
block_208:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_209
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_209
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_235
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_235
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_207
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_180
block_209:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_210
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_236
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_236
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_208
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_208
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_181
block_210:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_211
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_211
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_237
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_237
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_209
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_182
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_182
block_211:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_212
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_212
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_238
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_238
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_183
block_212:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_213
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_213
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_211
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_184
block_213:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_214
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_240
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_240
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_212
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_185
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_185
block_214:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_215
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_215
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_241
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_241
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_213
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_186
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
    jump block_242
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_242
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_214
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_214
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_187
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_187
block_216:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_243
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_243
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_188
block_221:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_222
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_222
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_247
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_247
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_194
block_222:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_223
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_223
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_248
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_248
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_221
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_195
block_223:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_224
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_224
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_249
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_249
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_222
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_222
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_196
block_224:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_225
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_225
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_250
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_250
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_223
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_223
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_197
block_225:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_226
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_226
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_251
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_251
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_224
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_224
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_198
block_226:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_227
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_227
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_252
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_252
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_225
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_225
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_199
block_227:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_228
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_228
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_253
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_253
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_226
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_200
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_200
block_228:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_229
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_229
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_254
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_254
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_227
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_227
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_201
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_201
block_229:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_230
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_230
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_255
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_255
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_228
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_228
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_202
block_230:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_231
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_231
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_256
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_256
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_229
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_203
block_231:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_232
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_232
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_257
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_257
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_230
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_204
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_204
block_232:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_233
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_233
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_258
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_258
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_231
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_231
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_205
block_233:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_234
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_234
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_259
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_232
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_206
block_234:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_235
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_235
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_260
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_260
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_233
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_233
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_207
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_207
block_235:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_236
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_236
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_261
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_261
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_234
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_234
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_208
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_208
block_236:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_237
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_237
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_262
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_262
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_235
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_209
block_237:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_238
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_238
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_263
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_236
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_210
block_238:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_239
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_264
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_237
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_237
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_211
block_239:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_240
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_240
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_265
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_265
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_238
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_238
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_212
block_240:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_241
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_241
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_266
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_213
block_241:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_242
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_242
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_267
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_267
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_240
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_240
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_214
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_214
block_242:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_243
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_243
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_268
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_268
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_241
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_241
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_215
block_243:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_269
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_269
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_242
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_242
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_216
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_216
block_247:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_248
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_248
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_221
block_248:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_249
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_249
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_247
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_247
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_222
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_222
block_249:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_250
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_250
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_248
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_248
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_223
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_223
block_250:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_251
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_251
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_249
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_249
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_224
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_224
block_251:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_252
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_252
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_250
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_225
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_225
block_252:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_253
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_253
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_251
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_251
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_226
block_253:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_254
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_254
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_252
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_252
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_227
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_227
block_254:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_255
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_255
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_253
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_228
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_228
block_255:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_256
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_256
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_254
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_254
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_229
block_256:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_257
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_257
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_255
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_255
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_230
block_257:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_258
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_258
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_256
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_256
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_231
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_231
block_258:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_259
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_257
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_257
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_232
block_259:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_260
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_260
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_258
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_258
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_233
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_233
block_260:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_261
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_261
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_259
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_234
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_234
block_261:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_262
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_262
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_260
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_260
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_235
block_262:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_263
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_261
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_261
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_236
block_263:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_264
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_262
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_262
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_237
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_237
block_264:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_265
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_265
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_263
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_238
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_238
block_265:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_266
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_264
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_239
block_266:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_267
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_267
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_265
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_265
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_240
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_240
block_267:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_268
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_268
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_266
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_241
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_241
block_268:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_269
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_269
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_267
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_267
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_242
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_242
block_269:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_268
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_268
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_243
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_243
