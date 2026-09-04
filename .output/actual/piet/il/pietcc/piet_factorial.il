; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_1
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (number)
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
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
    duplicate
    jump block_3
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_3
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_1
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_1
block_3:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_4
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_2
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_3
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_3
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_6
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_4
block_6:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_8
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_8
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_19
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_5
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_5
block_8:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_9
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_9
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_6
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_6
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (number)
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (number)
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_8
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_8
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_12
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_17
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    multiply
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    multiply
    jump block_9
block_12:
    switch_dpcc
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mod
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    mod
    jump block_12
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mod
    jump block_12
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_6
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_8
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    duplicate
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    duplicate
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_6
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_6
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (number)
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    divide
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    divide
    jump block_19
block_24:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    mod
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    mod
    jump block_20
block_26:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_20
block_28:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_26
block_29:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    multiply
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    multiply
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_28
block_30:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 2
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 2
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (number)
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (number)
    jump block_29
block_31:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_30
block_32:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_31
block_33:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out (char)
    jump block_35
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    out (char)
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_2
block_34:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_36
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    jump block_32
block_35:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    subtract
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    subtract
    jump block_33
block_36:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_34
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_39
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_35
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_35
block_39:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_36
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_36
