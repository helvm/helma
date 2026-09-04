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
    duplicate
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
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
    divide
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_2
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
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
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_22
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
    in (number)
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_22
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
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_6
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
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_22
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
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_9
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_12
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
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
    roll
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
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
    pointer
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_12
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_13
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_14
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_15
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
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
    add
    jump block_19
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_17
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_18
block_20:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_19
block_22:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_13
block_24:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_20
block_25:
    switch_dpcc
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_22
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_22
block_30:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_27
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_46
block_31:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_24
block_32:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_25
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_25
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_35
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_22
block_35:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_34
block_36:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_31
block_37:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_32
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_34
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_49
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_35
block_42:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_36
block_43:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_46
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_37
block_45:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_42
block_46:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_43
block_49:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_50
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_40
block_50:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_53
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_45
block_51:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_54
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_46
block_52:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_53
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_53
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_49
block_53:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_63
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_50
block_54:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_51
block_62:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_63
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_52
block_63:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_53
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_54
block_70:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_63
block_71:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_72
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_64
block_72:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_30
block_74:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_75
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_85
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_62
block_75:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_70
block_76:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_80
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_71
block_79:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_75
block_80:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_76
block_81:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_79
block_82:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_80
block_85:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_82
block_86:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_85
block_90:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_81
block_91:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_94
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_86
block_93:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_90
block_94:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_91
block_96:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_97
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_93
block_97:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_96
block_98:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_97
block_99:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_94
block_100:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_101
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_99
block_101:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_102
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_100
block_102:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_103
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_101
block_103:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_104
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_102
block_104:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_105
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_103
block_105:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_106
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_104
block_106:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_107
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_105
block_107:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_74
block_108:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_109
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_74
block_109:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_110
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_74
block_110:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_111
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_74
block_111:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_112
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_74
block_112:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_113
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_74
block_113:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_114
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_112
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_74
block_114:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_115
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_113
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_74
block_115:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_116
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_74
block_116:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_117
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_115
block_117:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_118
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_116
block_118:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_119
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_119
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_117
block_119:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_120
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_120
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_118
block_120:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_121
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_119
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_119
block_121:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_122
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_120
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_120
block_122:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_98
