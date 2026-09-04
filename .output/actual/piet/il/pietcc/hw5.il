; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_2
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_19
block_2:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
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
    divide
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_26
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
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_27
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
    duplicate
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
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
    duplicate
    jump block_7
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_7
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_5
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
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
    add
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_7
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
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
    add
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
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
    duplicate
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_10
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_11
block_13:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_47
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
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
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_13
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_14
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
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
    pointer
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_16
block_18:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_35
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_36
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_54
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_0
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_19
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_0
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_23
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_24
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_25
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_58
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_23
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_2
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_27
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_25
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_3
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_38
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_4
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_5
block_29:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_6
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_6
block_30:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_31
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_7
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_7
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_32
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_8
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_8
block_32:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_9
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_9
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_34
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_10
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_35
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_11
block_35:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_46
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_12
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_12
block_36:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_37
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_25
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_25
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_38
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_38
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_26
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_39
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_27
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_28
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_41
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_63
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_29
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_42
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_30
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_31
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_32
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_45
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_67
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_67
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_33
block_45:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_46
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_46
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_68
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_34
block_46:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_47
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_47
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_69
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_35
block_47:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_48
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_13
block_48:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_49
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_82
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_47
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_14
block_49:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_50
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_83
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_15
block_50:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_51
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_16
block_51:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_52
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_18
block_52:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_85
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_18
block_53:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_70
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_19
block_54:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_55
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_53
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_19
block_55:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_56
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_89
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_21
block_56:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_57
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_22
block_57:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_58
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_58
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_91
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_23
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_23
block_58:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_59
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_92
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_24
block_59:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_60
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_36
block_60:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_37
block_61:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_62
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_38
block_62:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_63
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_63
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_73
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_39
block_63:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_64
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_40
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_65
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_41
block_65:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_66
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_42
block_66:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_67
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_67
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_43
block_67:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_68
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_68
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_78
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_44
block_68:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_69
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_69
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_67
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_67
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_45
block_69:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_52
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_80
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_80
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_46
block_70:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_71
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_59
block_71:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_72
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_94
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_94
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_60
block_72:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_73
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_95
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_95
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_61
block_73:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_62
block_74:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_75
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_97
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_97
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_73
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_63
block_75:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_76
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_98
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_64
block_76:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_65
block_77:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_78
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_78
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_66
block_78:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_67
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_67
block_79:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_80
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_80
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_68
block_80:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_81
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_103
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_69
block_81:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_82
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_80
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_47
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_47
block_82:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_83
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_116
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_48
block_83:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_84
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_84
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_82
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_49
block_84:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_85
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_83
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_50
block_85:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_86
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_52
block_86:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_119
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_52
block_87:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_104
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_53
block_88:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_89
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_89
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_87
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_87
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_87
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_53
block_89:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_90
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_123
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_123
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_55
block_90:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_91
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_124
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_124
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_56
block_91:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_92
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_92
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_125
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_125
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_57
block_92:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_91
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_58
block_93:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_94
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_94
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_92
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_70
block_94:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_95
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_95
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_105
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_93
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_71
block_95:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_96
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_94
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_72
block_96:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_97
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_97
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_107
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_95
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_73
block_97:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_98
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_74
block_98:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_99
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_109
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_97
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_75
block_99:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_100
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_98
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_76
block_100:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_101
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_77
block_101:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_102
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_112
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_78
block_102:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_103
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_103
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_79
block_103:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_86
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_80
block_104:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_105
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_105
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_87
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_93
block_105:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_106
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_94
block_106:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_107
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_107
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_129
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_105
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_95
block_107:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_130
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_130
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_96
block_108:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_109
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_109
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_131
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_97
block_109:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_110
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_132
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_132
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_98
block_110:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_111
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_133
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_99
block_111:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_112
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_112
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_134
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_100
block_112:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_113
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_135
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_101
block_113:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_114
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_136
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_112
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_102
block_114:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_137
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_113
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_103
block_115:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_116
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_116
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_81
block_116:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_117
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_150
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_82
block_117:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_118
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_151
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_83
block_118:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_119
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_119
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_152
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_152
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_117
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_84
block_119:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_120
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_120
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_86
block_120:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_153
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_137
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_137
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_86
block_121:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_138
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_156
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_87
block_122:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_123
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_123
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_87
block_123:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_124
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_124
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_157
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_157
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_89
block_124:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_125
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_125
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_123
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_123
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_90
block_125:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_126
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_159
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_124
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_91
block_126:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_127
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_125
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_92
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_92
block_127:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_128
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_126
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_104
block_128:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_129
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_129
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_139
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_139
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_105
block_129:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_130
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_130
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_140
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_106
block_130:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_131
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_131
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_141
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_107
block_131:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_132
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_132
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_142
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_130
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_108
block_132:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_133
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_143
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_109
block_133:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_134
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_134
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_144
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_132
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_132
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_110
block_134:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_135
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_145
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_145
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_133
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_111
block_135:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_136
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_146
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_146
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_112
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_112
block_136:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_137
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_147
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_147
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_135
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_135
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_113
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_113
block_137:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_120
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_148
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_148
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_136
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_114
block_138:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_139
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_139
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_161
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_127
block_139:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_140
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_140
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_162
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_162
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_128
block_140:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_141
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_141
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_163
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_163
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_139
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_129
block_141:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_142
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_142
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_164
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_164
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_140
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_130
block_142:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_143
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_143
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_165
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_165
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_141
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_131
block_143:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_144
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_144
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_166
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_166
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_132
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_132
block_144:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_145
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_145
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_167
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_143
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_133
block_145:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_146
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_146
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_168
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_168
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_144
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_134
block_146:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_147
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_147
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_169
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_169
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_145
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_145
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_135
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_135
block_147:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_148
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_148
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_170
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_170
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_146
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_146
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_136
block_148:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_171
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_171
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_147
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_137
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_137
block_149:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_150
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_150
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_183
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_183
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_148
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_115
block_150:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_151
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_151
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_184
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_184
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_116
block_151:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_152
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_152
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_150
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_117
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_117
block_152:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_153
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_153
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_186
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_186
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_151
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_151
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_118
block_153:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_154
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_154
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_152
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_152
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_120
block_154:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_187
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_171
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_171
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_120
block_155:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_172
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_172
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_190
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_121
block_156:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_157
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_157
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_155
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_155
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_155
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_121
block_157:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_158
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_158
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_191
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_191
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_156
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_156
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_123
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_123
block_158:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_159
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_159
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_192
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_192
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_157
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_157
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_124
block_159:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_160
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_160
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_193
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_193
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_158
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_125
block_160:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_161
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_161
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_194
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_159
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_159
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_126
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_126
block_161:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_162
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_162
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_172
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_172
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_160
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_160
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_138
block_162:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_163
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_163
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_173
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_173
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_161
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_161
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_139
block_163:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_164
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_164
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_174
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_162
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_162
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_140
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_140
block_164:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_165
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_165
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_175
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_175
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_163
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_163
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_141
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_141
block_165:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_166
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_166
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_176
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_164
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_164
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_142
block_166:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_167
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_167
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_177
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_177
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_165
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_143
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_143
block_167:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_168
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_168
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_178
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_178
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_166
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_166
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_144
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_144
block_168:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_169
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_169
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_179
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_167
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_145
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_145
block_169:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_170
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_170
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_180
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_180
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_168
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_168
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_146
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_146
block_170:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_171
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_171
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_181
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_181
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_169
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_147
block_171:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_154
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_154
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_182
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_170
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_148
block_172:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_173
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_173
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_456
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_456
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_155
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_161
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_161
block_173:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_174
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_174
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_457
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_457
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_172
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_172
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_162
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_162
block_174:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_175
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_175
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_482
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_482
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_173
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_173
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_163
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_163
block_175:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_176
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_176
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_483
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_483
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_174
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_164
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_164
block_176:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_177
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_177
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_458
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_458
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_175
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_165
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_165
block_177:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_178
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_178
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_459
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_459
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_176
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_166
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_166
block_178:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_179
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_179
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_484
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_484
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_177
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_177
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_167
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_167
block_179:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_180
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_180
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_485
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_485
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_178
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_168
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_168
block_180:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_181
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_181
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_460
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_460
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_179
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_169
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_169
block_181:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_182
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_182
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_461
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_461
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_180
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_170
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_170
block_182:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_183
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_183
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_486
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_486
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_181
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_171
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_171
block_183:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_184
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_184
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_215
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_182
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_182
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_149
block_184:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_185
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_185
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_216
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_216
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_183
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_150
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_150
block_185:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_186
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_186
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_217
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_217
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_184
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_151
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_151
block_186:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_187
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_187
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_218
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_218
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_185
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_185
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_152
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_152
block_187:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_188
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_188
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_186
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_154
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_154
block_188:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_219
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_204
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_204
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_154
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_154
block_189:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_205
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_222
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_155
block_190:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_191
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_191
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_189
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_189
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_189
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_155
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_155
block_191:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_192
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_192
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_223
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_223
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_190
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_190
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_157
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_157
block_192:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_193
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_193
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_224
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_224
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_191
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_191
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_158
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_158
block_193:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_194
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_194
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_225
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_225
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_192
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_192
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_159
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_159
block_194:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_195
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_195
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_226
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_226
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_193
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_193
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_160
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_160
block_195:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_196
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_196
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_205
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_194
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_196:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_197
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_197
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_206
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_195
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_197:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_198
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_198
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_207
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_196
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_198:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_199
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_199
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_208
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_208
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_197
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_199:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_200
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_200
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_209
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_209
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_198
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_200:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_201
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_201
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_199
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_201:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_202
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_202
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_211
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_211
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_200
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_200
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_202:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_203
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_203
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_212
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_212
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_201
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_201
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_203:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_204
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_204
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_213
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_213
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_202
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_204:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_188
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_188
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_214
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_203
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_205:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_206
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_206
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_227
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_227
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_189
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_195
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_195
block_206:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_207
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_207
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_228
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_228
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_205
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_196
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_196
block_207:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_208
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_208
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_229
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_229
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_206
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_197
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_197
block_208:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_209
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_209
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_230
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_230
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_207
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_207
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_198
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_198
block_209:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_210
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_210
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_231
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_231
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_208
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_208
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_199
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_199
block_210:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_211
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_211
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_232
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_232
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_209
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_200
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_200
block_211:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_212
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_212
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_233
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_233
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_210
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_201
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_201
block_212:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_213
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_213
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_234
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_234
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_211
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_202
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_202
block_213:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_214
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_214
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_235
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_235
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_212
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_203
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_203
block_214:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_215
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_215
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_236
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_236
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_213
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_204
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_204
block_215:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_216
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_216
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_247
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_247
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_214
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_214
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_183
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_183
block_216:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_217
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_217
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_248
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_248
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_215
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_184
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_184
block_217:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_218
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_218
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_249
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_249
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_216
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_216
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_185
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_185
block_218:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_219
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_219
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_250
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_250
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_217
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_217
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_186
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_186
block_219:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_220
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_220
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_218
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_218
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_188
block_220:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_252
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_251
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_236
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_188
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_188
block_221:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_237
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_237
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_254
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_189
block_222:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_223
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_223
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_221
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_221
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_221
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_189
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_189
block_223:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_224
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_224
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_255
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_255
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_222
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_222
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_191
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_191
block_224:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_225
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_225
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_256
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_256
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_223
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_223
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_192
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_192
block_225:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_226
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_226
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_257
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_257
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_224
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_224
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_193
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_193
block_226:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_227
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_227
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_258
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_258
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_225
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_225
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_194
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_194
block_227:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_228
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_228
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_237
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_237
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_226
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_205
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_205
block_228:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_229
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_229
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_238
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_238
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_227
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_227
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_206
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_206
block_229:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_230
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_230
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_228
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_228
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_207
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_207
block_230:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_231
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_231
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_240
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_240
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_229
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_208
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_208
block_231:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_232
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_232
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_241
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_241
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_230
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_209
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_209
block_232:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_233
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_233
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_242
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_242
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_231
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_231
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_210
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_210
block_233:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_234
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_234
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_243
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_243
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_232
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_211
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_211
block_234:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_235
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_235
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_244
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_244
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_233
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_233
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_212
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_212
block_235:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_236
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_236
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_245
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_245
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_234
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_234
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_213
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_213
block_236:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_220
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_220
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_246
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_246
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_235
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_214
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_214
block_237:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_238
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_238
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_259
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_221
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_227
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_227
block_238:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_239
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_239
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_260
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_260
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_237
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_237
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_228
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_228
block_239:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_240
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_240
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_261
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_261
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_238
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_238
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_229
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_229
block_240:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_241
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_241
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_262
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_262
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_239
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_230
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_230
block_241:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_242
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_242
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_263
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_240
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_240
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_231
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_231
block_242:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_243
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_243
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_264
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_241
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_241
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_232
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_232
block_243:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_244
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_244
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_265
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_265
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_242
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_242
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_233
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_233
block_244:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_245
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_245
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_266
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_243
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_243
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_234
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_234
block_245:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_246
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_246
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_267
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_267
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_244
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_244
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_235
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_235
block_246:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_247
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_247
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_268
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_268
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_245
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_245
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_236
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_236
block_247:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_248
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_248
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_246
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_246
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_215
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_215
block_248:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_249
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_249
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_280
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_280
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_247
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_247
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_216
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_216
block_249:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_250
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_250
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_281
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_281
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_248
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_248
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_217
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_217
block_250:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_251
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_251
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_282
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_282
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_249
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_249
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_218
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_218
block_251:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_252
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_252
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_252
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_252
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_250
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_220
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_220
block_252:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_284
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_283
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_268
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_268
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_220
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_220
block_253:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_269
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_269
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_286
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_285
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_221
block_254:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_255
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_255
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_253
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_253
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_253
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_221
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_221
block_255:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_256
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_256
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_287
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_287
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_254
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_254
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_223
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_223
block_256:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_257
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_257
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_288
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_288
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_255
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_255
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_224
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_224
block_257:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_258
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_258
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_289
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_289
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_256
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_256
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_225
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_225
block_258:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_259
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_259
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_290
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_290
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_257
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_257
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_226
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_226
block_259:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_260
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_260
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_269
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_269
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_258
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_258
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_237
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_237
block_260:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_261
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_261
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_270
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_270
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_259
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_259
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_238
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_238
block_261:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_262
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_262
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_271
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_271
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_260
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_260
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_239
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_239
block_262:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_263
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_263
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_272
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_272
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_261
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_261
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_240
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_240
block_263:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_264
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_264
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_273
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_273
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_262
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_262
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_241
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_241
block_264:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_265
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_265
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_274
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_274
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_263
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_263
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_242
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_242
block_265:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_266
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_266
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_275
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_275
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_264
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_264
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_243
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_243
block_266:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_267
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_267
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_276
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_276
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_265
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_265
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_244
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_244
block_267:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_268
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_268
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_277
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_277
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_266
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_266
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_245
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_245
block_268:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_252
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_252
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_278
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_278
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_267
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_267
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_246
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_246
block_269:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_270
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_270
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_291
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_291
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_253
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_259
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_259
block_270:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_271
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_271
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_292
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_292
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_269
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_269
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_260
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_260
block_271:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_272
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_272
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_293
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_293
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_270
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_270
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_261
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_261
block_272:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_273
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_273
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_294
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_294
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_271
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_271
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_262
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_262
block_273:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_274
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_274
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_295
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_295
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_272
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_272
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_263
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_263
block_274:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_275
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_275
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_296
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_296
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_273
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_273
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_264
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_264
block_275:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_276
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_276
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_297
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_297
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_274
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_274
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_265
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_265
block_276:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_277
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_277
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_298
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_298
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_275
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_275
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_266
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_266
block_277:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_278
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_278
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_299
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_299
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_276
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_276
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_267
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_267
block_278:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_279
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_300
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_300
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_277
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_277
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_268
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_268
block_279:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_280
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_280
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_311
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_311
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_278
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_278
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_247
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_247
block_280:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_281
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_281
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_312
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_312
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_279
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_279
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_248
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_248
block_281:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_282
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_282
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_313
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_313
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_280
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_280
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_249
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_249
block_282:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_283
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_283
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_314
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_314
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_281
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_281
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_250
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_250
block_283:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_284
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_284
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_284
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_284
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_282
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_282
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_252
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_252
block_284:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_316
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_315
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_300
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_300
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_252
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_252
block_285:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_301
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_301
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_318
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_317
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_253
block_286:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_287
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_287
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_285
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_285
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_285
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_285
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_253
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_253
block_287:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_288
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_288
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_319
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_319
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_286
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_286
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_255
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_255
block_288:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_289
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_289
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_320
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_320
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_287
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_287
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_256
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_256
block_289:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_290
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_290
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_321
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_321
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_288
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_288
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_257
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_257
block_290:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_291
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_291
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_322
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_322
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_289
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_289
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_258
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_258
block_291:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_292
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_292
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_301
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_301
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_290
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_290
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_269
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_269
block_292:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_293
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_293
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_302
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_302
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_291
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_291
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_270
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_270
block_293:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_294
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_294
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_303
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_303
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_292
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_292
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_271
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_271
block_294:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_295
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_295
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_304
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_304
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_293
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_293
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_272
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_272
block_295:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_296
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_296
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_305
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_305
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_294
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_294
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_273
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_273
block_296:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_297
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_297
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_306
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_306
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_295
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_295
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_274
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_274
block_297:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_298
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_298
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_307
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_307
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_296
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_296
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_275
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_275
block_298:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_299
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_299
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_308
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_308
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_297
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_297
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_276
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_276
block_299:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_300
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_300
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_309
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_309
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_298
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_298
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_277
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_277
block_300:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_284
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_284
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_310
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_310
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_299
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_299
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_278
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_278
block_301:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_302
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_302
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_323
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_323
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_285
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_285
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_291
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_291
block_302:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_303
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_303
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_324
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_324
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_301
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_301
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_292
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_292
block_303:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_304
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_304
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_325
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_325
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_302
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_302
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_293
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_293
block_304:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_305
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_305
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_326
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_326
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_303
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_303
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_294
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_294
block_305:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_306
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_306
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_327
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_327
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_304
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_304
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_295
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_295
block_306:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_307
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_307
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_328
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_328
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_305
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_305
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_296
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_296
block_307:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_308
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_308
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_329
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_329
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_306
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_306
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_297
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_297
block_308:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_309
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_309
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_330
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_330
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_307
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_307
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_298
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_298
block_309:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_310
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_310
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_331
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_331
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_308
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_308
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_299
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_299
block_310:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_311
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_311
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_332
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_332
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_309
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_309
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_300
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_300
block_311:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_312
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_312
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_343
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_343
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_310
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_310
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_279
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_279
block_312:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_313
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_313
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_344
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_344
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_311
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_311
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_280
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_280
block_313:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_314
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_314
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_345
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_345
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_312
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_312
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_281
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_281
block_314:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_315
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_315
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_346
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_346
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_313
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_313
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_282
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_282
block_315:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_316
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_316
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_316
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_316
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_314
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_314
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_284
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_284
block_316:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_347
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_332
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_332
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_284
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_284
block_317:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_333
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_333
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_350
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_349
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_285
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_285
block_318:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_319
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_319
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_317
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_317
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_317
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_317
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_285
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_285
block_319:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_320
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_320
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_351
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_351
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_318
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_318
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_287
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_287
block_320:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_321
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_321
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_352
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_352
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_319
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_288
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_288
block_321:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_322
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_322
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_353
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_353
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_320
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_320
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_289
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_289
block_322:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_323
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_323
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_354
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_354
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_321
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_321
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_290
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_290
block_323:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_324
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_324
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_333
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_333
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_322
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_322
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_301
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_301
block_324:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_325
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_325
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_334
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_334
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_323
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_323
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_302
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_302
block_325:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_326
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_326
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_335
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_335
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_324
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_324
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_303
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_303
block_326:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_327
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_327
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_336
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_336
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_325
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_325
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_304
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_304
block_327:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_328
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_328
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_337
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_337
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_326
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_326
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_305
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_305
block_328:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_329
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_329
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_338
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_338
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_327
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_327
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_306
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_306
block_329:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_330
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_330
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_339
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_339
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_328
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_328
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_307
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_307
block_330:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_331
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_331
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_340
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_340
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_329
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_329
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_308
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_308
block_331:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_332
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_332
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_341
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_341
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_330
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_330
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_309
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_309
block_332:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_316
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_316
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_342
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_342
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_331
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_331
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_310
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_310
block_333:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_334
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_334
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_487
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_487
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_317
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_317
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_323
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_323
block_334:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_335
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_335
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_462
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_462
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_333
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_333
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_324
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_324
block_335:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_336
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_336
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_463
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_334
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_334
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_325
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_325
block_336:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_337
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_337
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_488
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_488
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_335
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_335
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_326
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_326
block_337:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_338
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_338
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_489
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_489
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_336
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_336
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_327
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_327
block_338:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_339
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_339
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_464
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_464
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_337
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_337
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_328
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_328
block_339:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_340
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_340
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_465
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_465
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_338
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_338
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_329
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_329
block_340:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_341
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_341
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_490
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_490
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_339
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_339
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_330
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_330
block_341:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_342
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_342
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_491
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_491
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_340
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_340
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_331
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_331
block_342:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_343
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_343
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_466
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_466
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_341
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_341
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_332
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_332
block_343:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_344
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_344
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_369
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_369
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_342
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_342
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_311
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_311
block_344:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_345
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_345
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_370
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_343
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_343
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_312
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_312
block_345:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_346
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_346
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_371
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_371
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_344
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_344
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_313
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_313
block_346:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_347
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_347
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_372
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_372
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_345
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_345
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_314
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_314
block_347:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_348
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_348
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_346
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_346
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_316
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_316
block_348:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_374
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_373
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_361
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_316
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_316
block_349:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_362
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_362
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_376
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_317
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_317
block_350:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_351
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_351
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_349
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_349
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_349
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_349
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_317
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_317
block_351:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_352
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_352
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_377
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_377
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_350
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_350
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_319
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_319
block_352:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_353
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_353
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_378
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_378
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_351
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_351
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_320
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_320
block_353:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_354
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_354
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_379
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_379
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_352
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_352
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_321
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_321
block_354:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_355
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_355
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_380
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_380
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_353
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_353
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_322
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_322
block_355:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_356
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_356
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_362
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_362
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_354
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_354
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_356:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_357
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_357
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_363
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_363
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_355
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_357:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_358
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_358
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_364
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_364
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_356
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_356
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_358:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_359
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_359
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_365
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_365
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_357
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_357
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_359:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_360
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_360
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_366
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_366
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_358
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_358
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_360:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_361
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_361
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_367
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_367
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_359
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_359
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_361:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_348
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_348
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_368
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_368
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_360
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_360
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_362:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_363
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_363
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_381
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_381
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_349
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_349
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_355
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_355
block_363:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_364
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_364
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_382
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_382
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_362
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_362
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_356
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_356
block_364:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_365
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_365
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_383
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_383
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_363
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_363
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_357
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_357
block_365:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_366
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_366
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_384
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_384
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_364
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_364
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_358
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_358
block_366:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_367
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_367
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_385
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_385
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_365
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_365
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_359
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_359
block_367:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_368
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_368
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_386
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_386
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_366
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_366
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_360
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_360
block_368:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_369
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_369
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_387
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_387
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_367
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_367
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_361
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_361
block_369:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_370
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_370
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_395
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_395
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_368
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_368
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_343
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_343
block_370:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_371
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_371
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_396
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_396
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_369
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_369
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_344
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_344
block_371:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_372
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_372
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_397
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_397
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_370
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_345
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_345
block_372:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_373
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_373
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_398
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_398
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_371
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_371
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_346
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_346
block_373:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_374
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_374
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_374
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_374
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_372
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_372
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_348
block_374:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_400
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_399
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_387
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_387
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_348
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_348
block_375:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_388
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_388
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_402
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_401
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_349
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_349
block_376:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_377
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_377
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_375
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_375
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_375
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_349
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_349
block_377:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_378
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_378
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_403
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_403
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_376
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_376
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_351
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_351
block_378:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_379
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_379
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_404
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_404
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_377
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_377
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_352
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_352
block_379:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_380
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_380
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_405
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_405
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_378
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_378
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_353
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_353
block_380:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_381
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_381
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_406
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_406
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_379
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_379
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_354
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_354
block_381:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_382
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_382
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_388
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_388
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_380
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_380
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_362
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_362
block_382:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_383
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_383
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_389
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_389
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_381
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_381
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_363
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_363
block_383:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_384
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_384
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_390
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_390
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_382
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_382
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_364
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_364
block_384:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_385
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_385
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_391
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_391
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_383
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_383
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_365
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_365
block_385:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_386
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_386
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_392
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_392
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_384
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_384
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_366
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_366
block_386:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_387
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_387
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_393
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_393
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_385
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_385
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_367
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_367
block_387:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_374
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_374
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_394
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_394
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_386
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_386
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_368
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_368
block_388:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_389
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_389
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_407
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_407
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_375
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_381
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_381
block_389:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_390
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_390
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_408
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_408
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_388
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_388
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_382
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_382
block_390:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_391
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_391
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_409
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_409
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_389
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_389
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_383
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_383
block_391:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_392
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_392
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_410
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_410
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_390
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_390
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_384
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_384
block_392:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_393
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_393
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_411
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_411
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_391
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_385
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_385
block_393:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_394
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_394
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_412
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_412
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_392
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_392
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_386
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_386
block_394:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_395
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_395
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_413
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_413
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_393
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_393
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_387
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_387
block_395:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_396
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_396
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_421
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_421
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_394
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_394
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_369
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_369
block_396:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_397
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_397
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_422
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_422
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_395
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_395
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_370
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_370
block_397:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_398
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_398
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_423
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_423
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_396
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_396
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_371
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_371
block_398:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_399
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_399
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_424
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_424
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_397
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_397
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_372
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_372
block_399:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_400
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_400
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_400
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_400
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_398
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_398
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_374
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_374
block_400:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_426
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_425
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_413
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_413
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_374
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_374
block_401:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_414
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_414
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_428
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_427
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_375
block_402:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_403
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_403
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_401
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_401
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_401
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_401
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_375
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_375
block_403:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_404
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_404
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_429
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_429
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_402
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_402
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_377
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_377
block_404:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_405
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_405
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_430
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_430
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_403
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_403
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_378
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_378
block_405:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_406
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_406
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_431
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_431
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_404
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_404
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_379
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_379
block_406:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_407
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_407
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_432
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_432
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_405
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_405
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_380
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_380
block_407:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_408
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_408
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_414
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_414
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_406
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_406
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_388
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_388
block_408:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_409
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_409
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_415
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_415
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_407
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_407
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_389
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_389
block_409:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_410
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_410
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_416
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_416
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_408
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_408
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_390
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_390
block_410:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_411
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_411
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_417
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_417
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_409
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_409
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_391
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_391
block_411:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_412
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_412
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_418
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_418
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_410
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_410
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_392
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_392
block_412:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_413
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_413
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_419
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_419
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_411
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_411
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_393
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_393
block_413:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_400
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_400
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_420
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_420
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_412
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_412
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_394
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_394
block_414:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_415
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_415
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_433
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_433
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_401
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_401
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_407
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_407
block_415:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_416
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_416
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_434
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_434
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_414
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_414
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_408
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_408
block_416:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_417
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_417
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_435
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_435
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_415
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_415
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_409
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_409
block_417:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_418
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_418
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_436
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_436
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_416
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_416
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_410
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_410
block_418:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_419
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_419
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_437
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_437
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_417
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_417
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_411
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_411
block_419:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_420
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_420
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_438
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_438
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_418
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_418
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_412
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_412
block_420:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_421
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_421
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_439
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_439
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_419
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_419
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_413
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_413
block_421:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_422
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_422
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_442
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_442
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_420
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_420
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_395
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_395
block_422:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_423
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_423
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_443
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_443
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_421
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_421
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_396
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_396
block_423:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_424
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_424
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_444
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_444
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_422
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_422
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_397
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_397
block_424:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_425
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_425
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_423
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_423
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_398
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_398
block_425:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_426
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_426
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_426
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_426
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_424
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_424
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_400
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_400
block_426:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_447
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_446
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_439
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_439
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_400
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_400
block_427:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_440
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_440
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_449
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_448
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_401
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_401
block_428:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_429
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_429
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_427
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_427
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_427
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_427
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_401
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_401
block_429:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_430
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_430
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_450
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_428
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_428
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_403
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_403
block_430:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_431
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_431
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_451
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_451
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_429
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_429
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_404
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_404
block_431:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_432
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_432
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_452
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_452
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_430
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_430
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_405
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_405
block_432:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_433
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_433
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_453
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_453
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_431
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_431
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_406
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_406
block_433:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_434
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_434
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_467
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_467
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_432
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_432
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_414
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_414
block_434:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_435
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_435
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_492
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_492
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_433
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_433
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_415
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_415
block_435:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_436
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_436
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_493
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_493
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_434
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_434
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_416
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_416
block_436:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_437
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_437
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_468
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_468
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_435
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_435
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_417
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_417
block_437:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_438
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_438
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_469
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_469
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_436
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_436
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_418
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_418
block_438:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_439
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_439
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_494
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_494
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_437
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_437
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_419
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_419
block_439:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_426
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_426
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_495
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_495
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_438
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_438
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_420
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_420
block_440:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_441
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_441
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_454
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_454
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_427
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_427
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_441:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_442
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_442
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_455
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_455
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_440
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_440
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_13
block_442:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_443
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_443
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_470
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_470
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_441
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_421
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_421
block_443:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_444
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_444
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_471
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_471
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_442
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_442
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_422
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_422
block_444:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_445
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_445
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_472
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_472
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_443
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_443
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_423
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_423
block_445:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_446
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_446
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_473
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_473
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_444
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_444
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_424
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_424
block_446:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_447
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_447
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_447
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_447
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_445
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_426
block_447:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_475
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_474
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_455
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_455
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_426
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_426
block_448:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_456
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_456
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_477
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_476
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_427
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_427
block_449:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_450
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_450
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_448
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_448
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_448
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_448
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_427
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_427
block_450:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_451
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_451
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_478
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_478
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_449
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_449
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_429
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_429
block_451:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_452
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_452
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_479
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_479
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_450
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_450
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_430
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_430
block_452:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_453
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_453
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_480
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_480
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_451
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_451
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_431
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_431
block_453:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_454
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_454
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_481
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_481
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_452
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_452
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_432
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_432
block_454:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_455
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_455
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_453
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_453
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_440
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_440
block_455:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_447
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_447
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_454
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_454
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_441
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_441
block_456:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_457
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_457
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_448
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_448
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_172
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_172
block_457:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_458
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_458
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_456
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_456
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_173
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_173
block_458:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_459
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_459
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_457
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_457
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_176
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_176
block_459:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_460
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_460
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_458
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_458
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_177
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_177
block_460:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_461
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_461
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_459
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_459
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_180
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_180
block_461:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_462
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_462
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_460
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_460
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_181
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_181
block_462:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_463
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_463
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_461
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_461
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_334
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_334
block_463:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_464
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_464
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_462
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_462
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_335
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_335
block_464:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_465
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_465
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_463
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_463
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_338
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_338
block_465:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_466
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_466
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_464
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_464
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_339
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_339
block_466:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_467
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_467
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_465
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_465
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_342
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_342
block_467:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_468
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_468
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_466
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_466
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_433
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_433
block_468:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_469
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_469
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_467
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_467
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_436
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_436
block_469:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_470
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_470
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_468
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_468
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_437
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_437
block_470:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_471
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_471
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_469
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_469
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_442
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_442
block_471:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_472
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_472
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_470
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_470
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_443
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_443
block_472:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_473
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_473
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_471
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_471
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_444
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_444
block_473:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_474
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_474
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_496
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_496
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_472
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_472
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_445
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_445
block_474:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_475
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_475
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_475
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_475
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_473
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_473
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_447
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_447
block_475:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_496
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_496
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_495
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_495
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_447
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_447
block_476:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_496
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_496
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_448
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_448
block_477:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_478
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_478
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_476
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_448
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_448
block_478:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_479
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_479
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_477
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_477
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_450
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_450
block_479:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_480
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_480
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_478
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_478
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_451
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_451
block_480:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_481
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_481
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_479
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_479
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_452
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_452
block_481:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_482
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_482
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_480
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_480
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_453
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_453
block_482:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_483
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_483
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_481
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_481
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_174
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_174
block_483:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_484
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_484
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_482
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_482
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_175
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_175
block_484:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_485
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_485
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_483
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_483
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_178
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_178
block_485:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_486
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_486
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_484
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_484
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_179
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_179
block_486:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_487
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_487
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_485
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_485
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_182
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_182
block_487:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_488
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_488
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_486
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_486
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_333
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_333
block_488:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_489
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_489
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_487
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_487
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_336
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_336
block_489:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_490
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_490
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_488
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_488
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_337
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_337
block_490:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_491
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_491
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_489
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_489
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_340
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_340
block_491:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_492
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_492
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_490
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_490
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_341
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_341
block_492:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_493
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_493
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_491
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_491
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_434
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_434
block_493:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_494
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_494
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_492
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_492
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_435
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_435
block_494:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_495
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_495
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_493
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_493
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_438
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_438
block_495:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_475
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_475
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_494
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_494
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_439
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_439
block_496:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_496
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_496
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_476
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_476
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_17
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
