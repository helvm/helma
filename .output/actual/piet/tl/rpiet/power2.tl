; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_37
block_3:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_4
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_0
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
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
    push 1
    jump block_7
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_7
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_5
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_6
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_9
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_9
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_7
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_9
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_12
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_10
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_11
block_13:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
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
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_13
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_14
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_15
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_16
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_17
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_18
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_20
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_23
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_33
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
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_22
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_23
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_24
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_26
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_27
block_29:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_28
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_38
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_11
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_34
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_23
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_24
block_36:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_29
block_37:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_0
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_31
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_21
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_34
block_41:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_58
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 12
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 12
    jump block_36
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_38
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_46
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_39
block_46:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_33
block_48:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_49
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    jump block_46
block_49:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_40
block_51:
    switch_dpcc
block_52:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_42
block_54:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_55
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_48
block_55:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_56
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_48
block_56:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_63
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_49
block_58:
    switch_dpcc
block_60:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_54
block_61:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_62
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_55
block_62:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 4
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_33
block_63:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_56
