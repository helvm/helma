; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_1
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_1
block_1:
    switch_dpcc
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_0
