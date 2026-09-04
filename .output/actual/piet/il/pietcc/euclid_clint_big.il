; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_3
block_3:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (number)
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (number)
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_0
block_4:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    jump block_11
block_5:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_6
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_6
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    jump block_3
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    jump block_4
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_9
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_6
block_9:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_8
block_11:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_4
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_9
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_23
block_14:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_11
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_24
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_15
block_19:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (number)
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (number)
    jump block_14
block_21:
    switch_dpcc
block_22:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_19
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_12
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_15
