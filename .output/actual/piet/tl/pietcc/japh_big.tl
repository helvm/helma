; --- PIET SWITCH-BASED ASSEMBLY LISTING ---
; Entry point: block_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

block_0:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 225
    jump block_1
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 225
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_27
block_1:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 49
    jump block_2
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 49
    jump block_2
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_29
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
    jump block_4
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 56
    jump block_14
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 56
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_1
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
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_43
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
    jump block_39
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_4
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_4
block_10:
    switch_dpcc
block_14:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 64
    jump block_15
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 64
    jump block_15
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_0
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_2
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_2
block_15:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_16
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_16
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_14
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
block_16:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_18
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_18
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_15
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_4
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_4
block_18:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 56
    jump block_19
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 56
    jump block_19
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    divide
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    divide
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_16
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_18
block_19:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_20
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_20
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_34
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_18
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_18
block_20:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    duplicate
    jump block_21
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    duplicate
    jump block_21
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_19
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_24
block_21:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_22
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_22
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    divide
    jump block_20
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    divide
    jump block_20
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_24
block_22:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    jump block_23
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    jump block_23
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_21
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_21
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_24
block_23:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_24
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_24
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    jump block_22
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    jump block_22
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_24
block_24:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_10
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_10
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_45
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_23
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_23
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_24
block_27:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    jump block_28
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    switch
    jump block_28
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_27
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_0
block_28:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_29
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_29
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_0
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_0
block_29:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_30
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_30
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 49
    jump block_28
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 49
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_1
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_1
block_30:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_31
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_31
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 56
    jump block_29
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 56
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 56
    jump block_14
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 56
    jump block_14
block_31:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_32
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_32
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_30
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_15
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_15
block_32:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_33
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_33
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 56
    jump block_31
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 56
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 56
    jump block_16
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 56
    jump block_16
block_33:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_34
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_34
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_32
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_32
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    duplicate
    jump block_18
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    duplicate
    jump block_18
block_34:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_45
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_45
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    jump block_37
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    jump block_37
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_33
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_33
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_19
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_19
block_37:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_53
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_53
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_40
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_34
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_34
block_39:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_40
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_40
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 56
    jump block_42
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 56
    jump block_42
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_5
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_5
block_40:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_39
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 161
    jump block_37
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_21
block_42:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_27
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_39
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_39
block_43:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4050
    jump block_53
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4050
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 4050
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_27
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 4050
    jump block_40
block_44:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_56
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 458
    jump block_55
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_56
block_45:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 56
    jump block_47
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 56
    jump block_47
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_24
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_24
block_47:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_45
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_45
block_48:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_49
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_49
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_57
block_49:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_50
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_50
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in (char)
    jump block_58
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    in (char)
    jump block_58
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 64
    jump block_48
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 64
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_28
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_28
block_50:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_51
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_51
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_49
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_30
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_30
block_51:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 56
    jump block_52
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 56
    jump block_52
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_61
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_61
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_50
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_50
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_31
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_31
block_52:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_51
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_43
block_53:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_44
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    jump block_63
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_53
block_55:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    jump block_56
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    jump block_56
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_44
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_56
block_56:
    switch_dpcc
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 64
    jump block_67
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 64
    jump block_67
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in (char)
    jump block_55
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in (char)
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_56
block_57:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    in (char)
    jump block_58
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    in (char)
    jump block_58
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 49
    jump block_48
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 49
    jump block_48
block_58:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_59
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_59
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    add
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    add
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    add
    jump block_49
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    add
    jump block_49
block_59:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_61
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 105
    jump block_58
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_29
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_29
block_61:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_59
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_59
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_51
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_51
block_62:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_63
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_63
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_43
block_63:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    subtract
    jump block_64
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    subtract
    jump block_64
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 56
    jump block_62
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 56
    jump block_62
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    jump block_53
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    jump block_53
block_64:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_66
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_66
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out (char)
    jump block_63
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out (char)
    jump block_63
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_44
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_44
block_66:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 49
    jump block_67
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 49
    jump block_67
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_64
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_64
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in (char)
    jump block_55
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    in (char)
    jump block_55
block_67:
    switch_dpcc
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    jump block_66
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    jump block_66
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_56
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_56
block_69:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    jump block_43
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    jump block_43
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    jump block_70
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    jump block_43
block_70:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    greater
    jump block_71
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    greater
    jump block_71
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_72
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_72
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 64
    jump block_69
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 64
    jump block_69
block_71:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_74
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    greater
    jump block_70
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    greater
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_43
block_72:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    jump block_74
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    jump block_74
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    jump block_75
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_70
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_70
block_74:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 401
    jump block_76
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_43
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_43
block_75:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out (char)
    jump block_76
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    out (char)
    jump block_76
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 56
    jump block_78
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 56
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_57
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_57
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    jump block_72
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    jump block_72
block_76:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCLeft}:
    subtract
    jump block_79
  case Course {directionPointer = DPDown, codelChooser = CCRight}:
    subtract
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    subtract
    jump block_75
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    subtract
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_74
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_74
block_78:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    jump block_79
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    jump block_75
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    jump block_75
block_79:
    switch_dpcc
  case Course {directionPointer = DPRight, codelChooser = CCLeft}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_79
  case Course {directionPointer = DPRight, codelChooser = CCRight}:
    store_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_79
  case Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 56
    jump block_78
  case Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 56
    jump block_78
  case Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out (char)
    jump block_76
  case Course {directionPointer = DPUp, codelChooser = CCRight}:
    out (char)
    jump block_76
