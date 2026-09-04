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
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_9
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_9
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    jump block_2
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_0
block_2:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_4
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_11
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    jump block_10
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_1
block_4:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_5
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_5
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_2
block_5:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_4
block_9:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_9
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_9
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_0
block_10:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_2
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_9
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    jump block_1
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    jump block_1
block_11:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_12
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_12
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_2
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
block_12:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_11
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_9
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_9
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_5
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 38
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_9
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
block_17:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_17
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_17
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_18
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_23
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_10
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_10
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_11
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_11
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_25
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_25
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_24
block_25:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    jump block_26
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    jump block_26
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_24
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_17
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
block_26:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_25
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_25
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
block_29:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_16
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_29
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_31
