; --- PIET FLAT ASSEMBLY LISTING ---
; Entry point: label_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

label_0:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_1
    push 72
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_1
    push 72
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_16
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_16
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_1:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_2
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_2
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_2
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_2
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_2:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_3
    push 101
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_4
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_16
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_0
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_0
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_0
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_3:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_4
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_4
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_4
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_4
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_2
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_2
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_4:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_20
    push 108
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_20
    push 108
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_2
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_2
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_13:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_15
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_15
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_8
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_8
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_15:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_26
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_28
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_28
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_0
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_13
    push 33
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_16:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_22
    push 100
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_15
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_2
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_2
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_18:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_35
    push 111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_35
    push 111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_16
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_4
    push 111
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_4
    push 111
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_20:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_21
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_21
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_18
    push 2
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_18
    push 2
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_18
    push 2
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_4
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_21:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_18
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_23
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_18
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_18
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_20
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_20
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_22:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_16
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_16
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_15
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_15
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_15
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_15
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_15
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_15
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_23:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_18
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_18
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_25
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_25
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_18
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_18
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_21
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_21
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_25:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_18
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_18
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_18
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_18
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_18
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_18
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_23
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_23
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_26:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_27
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_27
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_28
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_28
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_15
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_15
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_27:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_16
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_16
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_16
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_16
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_26
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_26
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_16
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_16
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_28:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_16
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_33
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_15
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_26
    push 114
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_31:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_18
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_34
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_33
    push 119
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_33
    push 119
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_16
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
label_33:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_31
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_31
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_38
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_28
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_34:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_18
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_36
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_31
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_31
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_18
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_18
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_35:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_36
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_36
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_34
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_34
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_18
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_18
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_36:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_37
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_37
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_34
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_34
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_35
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_35
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_37:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_34
    push 32
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_34
    push 32
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_34
    push 32
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_36
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_38:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_33
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_33
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_28
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_28
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_28
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_28
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
