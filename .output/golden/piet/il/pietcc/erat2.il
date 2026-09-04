; --- PIET FLAT ASSEMBLY LISTING ---
; Entry point: label_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

label_0:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_1
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_1
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_1:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_2
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_2
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_2:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_3
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_3
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_1
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_1
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_3:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_4
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_4
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_2
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_2
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_4:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_5
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_5
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_3
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_3
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_5:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_6
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_6
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_4
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_4
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_6:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_7
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_7
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_5
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_5
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_7:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_8
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_8
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_6
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_6
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_8:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_9
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_9
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_7
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_7
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_9:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_8
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_8
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_10:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_11
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_11
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_9
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_9
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_11:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_12
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_12
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_12:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_13
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_13
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_11
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_11
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_13:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_14
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_14
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_14
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_14
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_12
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_12
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_14:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_15
    push 28
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_18
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_15:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_16
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_16
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_18
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_18
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_14
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_14
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_16:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_17
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_17
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_18
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_18
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_15
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_15
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_17:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_18
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_18
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_18
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_18
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_16
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_16
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_18:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_19
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_14
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_14
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_19:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_20
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_20
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_18
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_18
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_20:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_21
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_21
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_19
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_19
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_21:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_22
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_22
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_22
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_22
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_20
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_20
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_22:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_23
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_23:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_24
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_24
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_22
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_22
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_24:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_25
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_25
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_26
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_26
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_23
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_23
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_25:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_26
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_26
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_26
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_26
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_24
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_24
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_26:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_27
    push 24
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_27:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_43
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_43
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_26
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_26
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_43:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_48
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_48
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_43
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_43
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_44:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_48
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_48
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_48
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_48
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_44
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_44
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_48:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_71
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_71
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_44
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_43
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_49:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_72
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_72
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_44
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_44
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_50:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_52
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_52
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_72
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_72
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_52:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_53
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_53
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_50
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_50
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_53:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_54
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_54
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_52
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_52
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_54:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_55
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_55
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_53
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_53
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_55:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_56
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_56
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_54
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_54
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_56:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_57
    push 16
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_57
    push 16
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_55
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_55
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_57:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_58
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_58
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_56
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_56
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_58:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_59
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_59
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_57
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_57
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_59:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_60
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_60
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_58
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_58
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_60:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_61
    push 16
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_61
    push 16
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_59
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_59
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_61:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_62
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_62
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_60
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_60
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_62:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_63
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_63
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_61
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_61
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_63:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_64
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_64
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_62
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_62
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_64:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_65
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_65
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_63
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_63
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_65:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_66
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_66
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_64
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_64
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_66:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_67
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_67
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_65
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_65
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_67:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_68
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_68
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_67
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_66
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_66
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_67
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_68:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_69
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_69
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_67
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_67
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_69:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_70
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_70
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_68
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_68
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_70:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_80
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_80
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_69
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_69
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_71:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_77
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_77
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_48
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_48
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_72:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_78
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_49
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_50
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_77:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_82
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_82
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_71
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_71
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_78:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_98
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_98
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_72
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_72
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_80:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_121
    push 16
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_70
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_81:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_82:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_81
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_81
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_81
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_81
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_77
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_77
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_98:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_120
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_120
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_78
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_78
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_120:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_139
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_139
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_98
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_98
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_121:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_156
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_156
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_80
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_80
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_122:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_157
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_81
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_81
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_139:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_164
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_164
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_120
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_120
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_156:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_166
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_166
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_121
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_121
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_157:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_168
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_168
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_122
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_122
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_164:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_213
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_213
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_139
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_139
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_165:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_214
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_214
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_166:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_165
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_165
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_165
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_165
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_156
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_156
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_167:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_216
    push 16
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_215
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_168:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_167
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_167
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_167
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_167
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_157
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_157
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_213:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_235
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_235
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_164
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_164
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_214:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_254
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_165
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_165
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_215:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_350
    push 28
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_167
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_167
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_216:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_255
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_255
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_215
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_215
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_167
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_167
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_235:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_264
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_264
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_213
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_213
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_254:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_265
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_265
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_214
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_214
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_255:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_266
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_266
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_215
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_215
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_216
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_216
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_264:
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_235
    push 20
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
label_265:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_301
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_301
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_254
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_254
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_266:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_215
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_215
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_215
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_215
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_255
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_255
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_301:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_310
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_310
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_265
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_265
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_309:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_264
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_264
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_329
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_329
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_264
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_264
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_310:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_348
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_348
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_301
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_301
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_329:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_359
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_359
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_309
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_309
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_348:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_361
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_360
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_310
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_350:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_373
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_373
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_215
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_215
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_359:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_374
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_374
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_329
    push 12
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_329
    push 12
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_360:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_67
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_67
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_348
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_348
    push 4
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_361:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_378
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_378
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_348
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_348
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_373:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_376
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_376
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_350
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_350
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_374:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_380
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_359
    push 16
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_359
    push 16
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_375:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_385
    push 36
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_385
    push 36
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_376:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_379
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_379
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_373
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_373
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_377:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_375
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_375
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_381
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_381
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_375
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_375
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_375
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_375
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_378:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_387
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_387
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_361
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_361
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_379:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_383
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_383
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_376
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_376
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_380:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_409
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_409
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_374
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_374
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_381:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_386
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_386
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_377
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_377
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_382:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_406
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_406
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_383:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_388
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_388
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_379
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_379
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_385:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_375
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_375
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_394
    push 20
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_386:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_390
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_390
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_381
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_381
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_387:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_382
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_382
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_382
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_382
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_378
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_388:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_391
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_391
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_383
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_383
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_389:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_392
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_389
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_380
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
label_390:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_395
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_395
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_386
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_386
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_391:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_408
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_408
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_388
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_388
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_392:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_393
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_393
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_389
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_389
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_393:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_394
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_394
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_392
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_392
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_394:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_385
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_385
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_393
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_393
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_395:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_410
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_410
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_390
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_390
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_396:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_413
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
label_397:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_398
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_398
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_396
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_396
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_396
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_396
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_398:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_399
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_399
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_397
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_397
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_399:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_400
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_400
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_398
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_398
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_400:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_401
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_401
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_399
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_399
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_401:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_402
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_402
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_400
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_400
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_402:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_403
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_403
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_401
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_401
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_403:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_404
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_404
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_402
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_402
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_404:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_405
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_405
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_403
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_403
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_405:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_406
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_406
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_404
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_404
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_406:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_405
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_405
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_407:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_415
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_415
    push 12
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_408:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_407
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_407
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_407
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_407
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_391
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_391
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_409:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_411
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_411
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_380
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_380
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_410:
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_395
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_411:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_416
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_416
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_409
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_409
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_412:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_410
    push 32
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_422
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_422
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_410
    push 32
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_413:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_423
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_396
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
label_414:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_414
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_414
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_424
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_424
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_415:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_425
    push 8
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_407
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_407
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_416:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_426
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_426
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_411
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_411
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_417:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_418
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_418
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_427
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_427
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_418:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_419
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_419
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_417
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_417
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_419:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_420
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_420
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_418
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_418
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_420:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_421
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_421
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_419
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_419
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_421:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_422
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_422
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_420
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_420
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_422:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_412
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_412
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_421
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_421
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_423:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_428
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_428
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_413
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_413
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_424:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_429
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_429
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_414
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_414
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_425:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_430
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_430
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_415
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_415
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_426:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_431
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_431
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_416
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_416
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_427:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_432
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_417
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_417
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_428:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_443
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_443
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_423
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_423
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_429:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_445
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_424
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_424
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_430:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_446
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_446
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_425
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_425
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_431:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_448
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_448
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_426
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_426
    push 8
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_432:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_433
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_433
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_427
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_427
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_433:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_434
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_434
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_432
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_432
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_434:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_435
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_435
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_433
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_433
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_435:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_436
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_436
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_434
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_434
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_436:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_437
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_437
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_435
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_435
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_437:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_438
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_438
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_436
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_436
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_438:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_439
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_439
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_437
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_437
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_439:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_440
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_440
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_438
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_438
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_440:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_441
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_441
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_439
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_439
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_441:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_442
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_442
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_440
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_440
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_442:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_443
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_443
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_441
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_441
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_443:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_442
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_428
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_445:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_465
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_463
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_429
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_446:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_447
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_447
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_430
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_430
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_447:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_451
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_451
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_446
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_446
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_448:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_452
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_452
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_431
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_431
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_449:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_472
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_471
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_450:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_474
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_451:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_450
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_450
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_450
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_450
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_447
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_447
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_452:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_453
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_453
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_448
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_448
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_453:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_454
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_454
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_452
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_452
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_454:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_455
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_455
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_453
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_453
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_455:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_456
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_456
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_454
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_454
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_456:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_457
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_457
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_455
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_455
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_457:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_458
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_458
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_456
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_456
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_458:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_459
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_459
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_457
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_457
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_459:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_460
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_460
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_458
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_458
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_460:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_461
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_461
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_459
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_459
    push 16
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_461:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_462
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_462
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_460
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_460
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_462:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_463
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_463
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_461
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_461
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_463:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_445
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_445
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_462
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_462
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_465:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_466
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_466
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_445
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_445
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_466:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_467
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_467
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_465
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_465
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_467:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_468
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_468
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_466
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_466
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_468:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_469
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_469
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_467
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_467
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_469:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_470
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_470
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_468
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_468
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_470:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_471
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_471
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_469
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_469
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_471:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_449
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_449
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_470
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_470
    push 4
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_472:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_473
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_473
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_449
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_449
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_473:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_474
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_474
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_472
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_472
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_474:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_450
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_450
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_473
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_473
    push 8
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
