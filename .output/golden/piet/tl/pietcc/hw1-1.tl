; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_1
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_13
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_2
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_0
block_2:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_15
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
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_2
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
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
    subtract
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
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
    switch
    jump block_19
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_5
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_20
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
    mod
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_7
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_9
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_12
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_10
block_12:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_11
block_13:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_0
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_1
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_1
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_2
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_3
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_4
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_19
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_19
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_17
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_5
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_6
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_6
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_7
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_7
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_8
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_8
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_23
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_9
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_9
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_24
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_10
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_25
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_23
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_11
block_25:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_38
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_12
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_12
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_27
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_13
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_14
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_15
block_29:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_16
block_30:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_31
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_17
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_17
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_32
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_18
block_32:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_19
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_34
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_46
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_20
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_35
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_47
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_21
block_35:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_36
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_22
block_36:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_37
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_23
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_23
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_38
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_38
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_24
block_38:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_25
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_25
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_26
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_41
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_41
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_53
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_27
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_42
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_54
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_28
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_29
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_56
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_30
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_45
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_31
block_45:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_46
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_32
block_46:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_47
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_33
block_47:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_48
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_48
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_58
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_46
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_34
block_48:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_49
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_47
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_35
block_49:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_50
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_36
block_50:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_51
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_37
block_51:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_38
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_38
block_52:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_53
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_53
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_63
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_39
block_53:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_54
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_54
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_40
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_40
block_54:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_55
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_53
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_41
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_41
block_55:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_56
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_54
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_42
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_42
block_56:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_43
block_58:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_59
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_47
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_47
block_59:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_60
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_60
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_68
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_48
block_60:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_69
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_49
block_61:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_62
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_62
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_60
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_50
block_62:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_71
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_51
block_63:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_64
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_52
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_52
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_65
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_65
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_73
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_53
block_65:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_66
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_54
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_54
block_66:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_65
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_55
block_67:
    switch_dpcc
block_68:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_69
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_69
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_59
block_69:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_70
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_68
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_60
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_60
block_70:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_71
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_78
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_69
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_61
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_61
block_71:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_62
block_72:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_73
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_73
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_80
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_63
block_73:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_64
block_74:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_75
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_82
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_73
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_65
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_65
block_75:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_83
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_66
block_76:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_77
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_77
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_85
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_68
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_68
block_77:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_78
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_78
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_69
block_78:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_87
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_87
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_77
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_70
block_79:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_88
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_71
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_71
block_80:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_81
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_81
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_89
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_72
block_81:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_82
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_82
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_80
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_73
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_73
block_82:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_83
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_83
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_91
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_81
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_74
block_83:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_92
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_82
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_75
block_84:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_94
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_67
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_67
block_85:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_86
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_86
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_76
block_86:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_87
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_87
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_97
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_97
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_85
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_77
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_77
block_87:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_88
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_88
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_98
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_86
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_78
block_88:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_99
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_87
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_79
block_89:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_90
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_90
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_100
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_80
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_80
block_90:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_91
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_91
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_89
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_81
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_81
block_91:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_92
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_92
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_90
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_82
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_82
block_92:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_93
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_93
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_103
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_91
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_83
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_83
block_93:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_92
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_92
block_94:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_84
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_84
block_95:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_96
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_96
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_108
block_96:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_97
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_97
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_109
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_95
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_85
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_85
block_97:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_98
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_98
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_96
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_86
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_86
block_98:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_99
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_99
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_97
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_87
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_87
block_99:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_112
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_112
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_98
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_88
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_88
block_100:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_101
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_101
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_113
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_113
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_89
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_89
block_101:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_102
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_102
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_100
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_90
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_90
block_102:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_103
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_103
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_101
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_91
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_91
block_103:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_104
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_104
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_116
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_102
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_92
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_92
block_104:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_105
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_105
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_103
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_93
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_93
block_105:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_106
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_104
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_104
block_106:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_107
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_119
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_131
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_118
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_94
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_94
block_107:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_108
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_108
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_119
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_119
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_106
block_108:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_109
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_109
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_120
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_107
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_95
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_95
block_109:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_110
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_110
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_108
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_96
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_96
block_110:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_111
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_111
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_109
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_97
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_97
block_111:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_112
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_112
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_123
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_123
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_110
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_98
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_98
block_112:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_124
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_124
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_111
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_99
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_99
block_113:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_114
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_114
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_125
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_100
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_100
block_114:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_115
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_115
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_113
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_113
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_101
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_101
block_115:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_116
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_116
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_114
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_102
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_102
block_116:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_117
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_117
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_115
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_103
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_103
block_117:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_118
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_118
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_129
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_116
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_104
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_104
block_118:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_106
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_106
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_130
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_130
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_117
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_117
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_105
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_105
block_119:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_120
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_120
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_132
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_132
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_106
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_107
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_107
block_120:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_121
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_121
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_133
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_119
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_108
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_108
block_121:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_122
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_122
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_134
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_120
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_109
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_109
block_122:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_123
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_123
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_135
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_121
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_110
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_110
block_123:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_124
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_124
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_136
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_122
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_111
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_111
block_124:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_137
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_123
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_123
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_112
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_112
block_125:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_126
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_126
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_138
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_113
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_113
block_126:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_127
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_127
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_139
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_139
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_125
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_114
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_114
block_127:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_128
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_128
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_140
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_126
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_126
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_115
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_115
block_128:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_129
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_129
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_141
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_127
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_116
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_116
block_129:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_130
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_130
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_142
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_128
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_117
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_117
block_130:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_131
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_131
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_143
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_129
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_118
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_118
block_131:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_132
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_132
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_144
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_130
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_106
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_106
block_132:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_133
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_133
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_145
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_145
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_131
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_119
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_119
block_133:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_134
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_134
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_146
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_146
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_132
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_132
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_120
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_120
block_134:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_135
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_135
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (number)
    jump block_147
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (number)
    jump block_147
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_133
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_121
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_121
block_135:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_136
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_136
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_148
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_148
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_134
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_122
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_122
block_136:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_137
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_137
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_149
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_135
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_135
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_123
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_123
block_137:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_150
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_136
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_124
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_124
block_138:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_139
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_125
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_125
block_139:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_140
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_138
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_138
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_126
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_126
block_140:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_141
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_139
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_139
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_127
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_127
block_141:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_142
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_140
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_140
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_128
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_128
block_142:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_143
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_141
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_141
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_129
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_129
block_143:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_144
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_142
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_142
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_130
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_130
block_144:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_145
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_145
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_143
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_143
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_131
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_131
block_145:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_146
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_146
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_144
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_144
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_132
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_132
block_146:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_147
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_147
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_145
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_145
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_133
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_133
block_147:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_148
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_148
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_146
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_146
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_134
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_134
block_148:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_149
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_147
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_147
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_135
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_135
block_149:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_150
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_150
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_148
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_148
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_136
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_136
block_150:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_149
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_149
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_137
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_137
