; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 11
    jump block_1
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
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
    push 2
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_1
block_3:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_4
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_2
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_3
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_6
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_4
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_7
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_7
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_5
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_6
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_7
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
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
    roll
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_9
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_10
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_13
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_11
block_13:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_14
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_12
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_13
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_14
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
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
    duplicate
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_16
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_17
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_18
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
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
    out (number)
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_20
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 43
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_21
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_24
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
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
    out (number)
    jump block_25
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_23
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 61
    jump block_26
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_27
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_25
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_26
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_27
block_29:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_28
block_30:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_46
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_29
block_36:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_38
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (number)
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (number)
    jump block_44
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_36
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_36
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_43
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 25
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_22
block_41:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 13
    jump block_42
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 13
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_36
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_41
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_41
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_39
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 9
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 9
    jump block_42
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_37
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_39
block_45:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_40
block_46:
    switch_dpcc
