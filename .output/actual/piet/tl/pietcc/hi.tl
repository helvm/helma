; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 9
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 9
    jump block_1
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_8
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    not
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    not
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_0
block_3:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 11
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 11
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_1
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_6
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_6
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    mod
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_1
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_3
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_7
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_7
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_3
block_7:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_12
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_6
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_3
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_13
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_17
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    roll
    jump block_1
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    roll
    jump block_1
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_5
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 10
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 10
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_6
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_6
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_7
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_7
block_13:
    switch_dpcc
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    jump block_10
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_11
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_13
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_13
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_11
block_16:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_19
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    multiply
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    multiply
    jump block_11
block_17:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_8
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_8
block_19:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_16
block_20:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_19
block_23:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_20
