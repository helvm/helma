; --- PIET FLAT ASSEMBLY LISTING ---
; Entry point: label_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

label_0:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_1
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_1
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_13
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_13
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_1:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_2
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_2
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_14
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_14
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_0
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_0
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_2:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_3
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_3
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_15
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_15
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_1
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_1
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_3:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_4
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_4
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_16
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_16
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_2
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_2
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_4:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_5
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_5
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_17
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_17
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_3
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_3
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_5:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_6
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_6
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_18
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_18
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_4
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_4
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_6:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_7
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_7
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_19
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_19
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_5
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_5
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_7:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_8
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_8
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_20
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_20
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_6
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_6
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_8:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_9
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_9
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_21
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_21
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_7
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_7
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_9:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_22
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_22
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_8
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_8
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_10:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_11
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_11
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_23
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_23
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_9
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_9
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_11:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_12
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_12
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_24
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_24
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_12:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_25
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_25
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_11
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_11
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_13:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_14
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_14
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_26
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_26
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_0
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_0
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_14:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_15
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_15
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_27
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_27
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_13
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_13
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_1
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_1
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_15:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_16
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_16
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_28
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_28
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_14
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_14
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_2
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_2
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_16:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_17
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_17
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_29
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_29
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_15
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_15
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_3
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_3
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_17:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_18
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_18
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_30
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_30
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_16
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_16
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_4
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_4
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_18:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_19
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_19
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_31
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_31
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_17
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_17
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_5
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_5
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_19:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_20
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_20
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_32
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_32
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_18
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_18
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_6
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_6
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_20:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_21
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_21
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_33
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_33
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_19
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_19
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_7
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_7
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_21:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_22
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_22
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_34
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_34
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_20
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_20
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_8
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_8
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_22:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_23
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_23
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_35
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_35
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_21
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_21
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_9
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_9
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_23:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_24
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_24
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_36
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_36
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_22
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_22
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_10
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_10
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_24:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_25
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_25
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_37
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_37
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_23
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_23
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_11
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_11
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_25:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_38
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_38
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_24
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_24
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_12
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_12
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_26:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_27
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_27
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_39
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_39
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_13
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_13
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_27:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_28
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_28
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_40
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_40
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_26
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_26
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_14
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_14
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_28:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_29
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_29
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_41
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_41
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_27
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_27
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_15
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_15
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_29:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_30
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_30
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_42
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_42
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_28
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_28
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_16
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_16
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_30:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_31
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_31
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_43
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_43
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_29
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_29
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_17
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_17
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_31:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_32
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_32
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_44
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_44
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_30
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_30
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_18
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_18
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_32:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_33
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_33
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_45
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_45
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_31
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_31
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_19
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_19
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_33:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_34
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_34
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_46
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_46
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_32
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_32
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_20
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_20
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_34:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_35
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_35
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_47
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_47
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_33
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_33
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_21
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_21
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_35:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_36
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_36
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_48
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_48
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_34
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_34
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_22
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_22
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_36:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_37
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_37
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_49
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_49
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_35
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_35
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_23
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_23
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_37:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_38
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_38
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_50
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_50
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_36
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_36
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_24
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_24
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_38:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_51
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_51
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_37
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_37
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_25
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_25
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_39:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_40
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_40
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_52
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_52
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_26
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_26
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_40:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_41
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_41
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_53
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_53
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_39
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_39
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_27
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_27
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_41:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_42
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_42
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_54
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_54
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_40
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_40
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_28
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_28
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_42:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_43
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_43
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_55
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_55
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_41
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_41
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_29
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_29
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_43:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_44
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_44
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_56
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_56
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_42
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_42
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_30
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_30
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_44:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_45
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_45
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_43
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_43
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_31
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_31
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_45:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_46
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_46
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_44
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_44
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_32
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_32
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_46:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_47
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_47
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_45
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_45
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_33
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_33
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_47:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_48
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_48
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_58
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_58
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_46
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_46
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_34
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_34
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_48:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_49
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_49
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_59
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_59
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_47
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_47
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_35
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_35
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_49:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_50
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_50
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_60
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_60
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_48
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_48
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_36
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_36
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_50:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_51
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_51
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_61
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_61
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_49
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_49
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_37
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_37
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_51:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_62
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_62
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_50
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_50
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_38
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_38
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_52:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_53
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_53
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_63
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_63
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_39
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_39
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_53:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_54
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_54
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_64
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_64
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_52
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_52
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_40
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_40
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_54:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_55
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_55
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_65
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_65
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_53
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_53
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_41
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_41
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_55:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_56
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_56
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_66
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_66
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_54
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_54
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_42
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_42
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_56:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_55
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_55
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_43
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_43
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_58:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_59
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_59
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_47
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_47
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_59:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_60
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_60
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_68
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_68
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_58
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_58
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_48
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_48
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_60:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_61
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_61
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_69
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_69
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_59
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_59
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_49
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_49
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_61:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_62
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_62
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_70
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_70
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_60
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_60
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_50
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_50
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_62:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_71
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_71
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_61
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_61
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_51
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_51
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_63:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_64
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_64
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_72
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_72
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_52
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_52
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_64:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_65
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_65
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_73
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_73
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_63
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_63
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_53
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_53
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_65:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_66
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_66
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_74
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_74
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_64
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_64
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_54
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_54
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_66:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_75
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_75
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_65
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_65
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_55
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_55
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_68:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_69
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_69
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_76
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_76
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_59
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_59
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_69:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_70
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_70
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_77
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_77
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_68
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_68
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_60
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_60
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_70:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_71
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_71
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_78
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_78
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_69
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_69
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_61
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_61
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_71:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_79
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_79
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_70
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_70
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_62
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_62
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_72:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_73
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_73
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_80
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_80
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_63
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_63
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_73:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_74
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_74
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_81
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_81
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_72
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_72
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_64
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_64
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_74:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_75
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_75
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_82
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_82
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_73
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_73
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_65
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_65
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_75:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_83
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_83
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_74
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_74
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_66
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_66
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_76:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_77
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_77
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_85
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_85
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_68
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_68
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_77:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_78
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_78
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_86
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_86
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_76
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_76
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_69
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_69
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_78:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_79
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_79
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_87
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_87
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_77
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_77
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_70
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_70
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_79:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_88
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_88
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_78
    in (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_78
    in (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_71
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_71
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_80:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_81
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_81
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_89
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_89
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_72
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_72
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_81:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_82
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_82
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_90
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_90
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_80
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_80
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_73
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_73
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_82:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_83
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_83
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_91
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_91
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_81
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_81
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_74
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_74
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_83:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_92
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_92
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_82
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_82
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_75
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_75
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_84:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_94
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_94
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_67
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_67
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_85:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_86
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_86
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_96
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_96
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_76
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_76
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_86:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_87
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_87
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_97
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_97
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_85
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_85
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_77
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_77
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_87:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_88
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_88
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_98
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_98
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_86
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_86
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_78
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_78
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_88:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_99
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_99
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_87
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_87
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_79
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_79
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_89:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_90
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_90
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_100
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_100
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_80
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_80
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_90:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_91
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_91
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_101
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_101
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_89
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_89
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_81
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_81
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_91:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_92
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_92
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_102
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_102
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_90
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_90
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_82
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_82
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_92:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_93
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_93
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_103
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_103
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_91
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_91
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_83
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_83
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_93:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_104
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_104
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_92
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_92
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_94:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_106
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_106
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_84
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_84
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_95:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_96
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_96
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_108
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_108
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_96:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_97
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_97
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_109
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_109
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_95
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_95
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_85
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_85
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_97:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_98
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_98
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_110
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_110
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_96
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_96
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_86
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_86
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_98:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_99
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_99
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_111
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_111
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_97
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_97
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_87
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_87
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_99:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_112
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_112
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_98
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_98
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_88
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_88
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_100:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_101
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_101
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_113
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_113
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_89
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_89
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_101:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_102
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_102
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_114
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_114
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_100
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_100
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_90
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_90
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_102:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_103
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_103
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_115
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_115
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_101
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_101
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_91
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_91
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_103:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_104
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_104
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_116
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_116
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_102
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_102
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_92
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_92
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_104:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_105
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_105
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_117
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_117
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_103
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_103
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_93
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_93
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_105:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_106
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_106
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_118
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_118
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_104
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_104
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_106:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_107
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_119
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_131
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_131
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_118
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_105
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_94
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_94
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_107:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_108
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_108
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_119
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_119
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_106
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_106
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_108:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_109
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_109
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_120
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_120
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_107
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_107
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_95
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_95
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_109:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_110
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_110
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_121
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_121
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_108
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_108
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_96
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_96
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_110:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_111
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_111
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_109
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_109
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_97
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_97
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_111:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_112
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_112
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_123
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_123
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_110
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_110
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_98
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_98
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_112:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_124
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_124
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_111
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_111
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_99
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_99
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_113:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_114
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_114
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_125
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_125
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_100
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_100
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_114:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_115
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_115
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_126
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_126
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_113
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_113
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_101
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_101
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_115:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_116
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_116
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_127
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_127
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_114
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_114
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_102
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_102
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_116:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_117
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_117
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_128
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_128
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_115
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_115
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_103
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_103
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_117:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_118
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_118
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_129
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_129
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_116
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_116
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_104
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_104
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_118:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_106
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_106
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_130
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_130
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_117
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_117
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_105
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_105
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_119:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_120
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_120
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_132
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_132
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_106
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_106
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_107
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_107
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_120:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_121
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_121
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_133
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_133
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_119
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_119
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_108
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_108
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_121:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_122
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_122
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_134
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_134
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_120
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_120
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_109
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_109
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_122:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_123
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_123
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_135
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_135
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_121
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_121
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_110
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_110
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_123:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_124
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_124
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_136
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_136
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_122
    in (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_122
    in (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_111
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_111
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_124:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_137
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_137
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_123
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_123
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_112
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_112
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_125:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_126
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_126
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_138
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_138
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_113
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_113
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_126:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_127
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_127
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_139
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_139
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_125
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_125
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_114
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_114
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_127:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_128
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_128
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_140
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_140
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_126
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_126
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_115
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_115
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_128:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_129
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_129
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_141
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_141
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_127
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_127
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_116
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_116
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_129:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_130
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_130
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_142
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_142
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_128
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_128
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_117
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_117
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_130:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_131
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_131
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_143
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_143
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_129
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_129
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_118
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_118
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_131:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_132
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_132
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_144
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_144
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_130
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_130
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_106
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_106
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_132:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_133
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_133
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_145
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_145
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_131
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_131
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_119
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_119
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_133:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_134
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_134
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_146
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_146
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_132
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_132
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_120
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_120
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_134:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_135
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_135
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_147
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_147
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_133
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_133
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_121
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_121
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_135:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_136
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_136
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_148
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_148
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_134
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_134
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_122
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_122
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_136:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_137
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_137
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_149
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_149
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_135
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_135
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_123
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_123
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_137:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_150
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_150
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_136
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_136
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_124
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_124
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_138:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_139
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_139
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_125
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_125
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_139:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_140
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_140
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_138
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_138
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_126
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_126
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_140:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_141
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_141
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_139
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_139
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_127
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_127
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_141:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_142
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_142
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_140
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_140
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_128
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_128
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_142:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_143
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_143
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_141
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_141
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_129
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_129
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_143:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_144
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_144
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_142
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_142
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_130
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_130
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_144:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_145
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_145
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_143
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_143
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_131
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_131
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_145:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_146
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_146
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_144
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_144
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_132
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_132
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_146:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_147
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_147
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_145
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_145
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_133
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_133
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_147:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_148
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_148
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_146
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_146
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_134
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_134
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_148:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_149
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_149
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_147
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_147
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_135
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_135
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_149:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_150
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_150
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_148
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_148
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_136
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_136
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_150:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_149
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_149
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_137
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_137
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
