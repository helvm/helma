; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    greater
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    greater
    jump block_1
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_1
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    greater
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    greater
    jump block_1
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    greater
    jump block_1
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    greater
    jump block_1
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    greater
    jump block_0
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    greater
    jump block_0
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    greater
    jump block_0
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    greater
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    greater
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    greater
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_0
