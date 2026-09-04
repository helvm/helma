; --- PIET ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_1
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_9
block_1:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_10
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_0
block_2:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_1
block_4:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_2
block_5:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_4
block_9:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_10
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_9
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_0
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_0
block_10:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_22
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_9
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_1
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_1
block_11:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_23
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_2
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
block_12:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
block_14:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_15
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_9
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_5
block_15:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
block_16:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 38
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_9
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_4
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
block_17:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_25
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
block_18:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
block_20:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_24
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_18
block_21:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_22
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
block_22:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_23
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_10
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_10
block_23:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_16
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_22
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_22
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_11
block_24:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_25
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_20
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_24
block_25:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_26
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_24
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_24
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
block_26:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_25
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_25
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_26
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_26
block_29:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_31
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_16
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_16
block_31:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_21
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_21
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_29
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_29
block_33:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_31
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_31
