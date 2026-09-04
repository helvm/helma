; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 72
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 72
    jump block_1
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_16
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_2
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_2
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
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
    push 101
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_4
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_0
block_3:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_4
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_4
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_4
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_2
block_4:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 108
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 108
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_2
block_8:
    switch_dpcc
block_13:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_8
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_8
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 33
    jump block_13
block_16:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 100
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_2
block_18:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 111
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 111
    jump block_35
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 111
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 111
    jump block_4
block_20:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 2
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_4
block_21:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_20
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_15
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_21
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_23
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_23
block_26:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_27
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_15
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_26
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_16
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 114
    jump block_26
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 119
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 119
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_16
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_31
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_28
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_18
block_35:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_36
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_18
block_36:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_35
block_37:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 32
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 32
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 32
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_36
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_28
