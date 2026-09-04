; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 10
    jump block_3
block_2:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_3
block_3:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_2
