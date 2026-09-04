; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 12
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 12
    jump block_5
block_2:
    switch_dpcc
block_3:
    switch_dpcc
block_4:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_3
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_3
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_4
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 2
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_2
