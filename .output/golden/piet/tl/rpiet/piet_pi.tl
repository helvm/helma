; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 52
    jump block_32
block_2:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8492
    jump block_40
block_32:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_0
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_34
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_32
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_33
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 10
    jump block_42
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 10
    jump block_42
block_38:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 10
    jump block_54
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 10
    jump block_54
block_39:
    switch_dpcc
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_37
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_2
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_37
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_42
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_45
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_43
block_45:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_46
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_44
block_46:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    multiply
    jump block_47
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    multiply
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_45
block_47:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_48
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (number)
    jump block_46
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (number)
    jump block_46
block_48:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_49
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_47
block_49:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    roll
    jump block_50
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    roll
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_48
block_50:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    divide
    jump block_51
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    divide
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    not
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    not
    jump block_49
block_51:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_52
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_52
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    duplicate
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    duplicate
    jump block_50
block_52:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_38
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_51
block_54:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_39
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_38
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_38
