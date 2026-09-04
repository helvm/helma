; --- PIET FLAT ASSEMBLY LISTING ---
; Entry point: label_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

label_0:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_1
    push 64
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_1
    push 64
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_1:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_2
    push 25
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_2:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_3
    push 7
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_3
    push 7
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_66
    pointer
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
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_48
    in (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_2
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_4:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_5
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_28
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_69
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_3
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_3
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_5:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_6
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_6
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_28
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_28
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_4
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_4
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_6:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_7
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_7
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_29
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_29
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_5
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_5
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_7:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_8
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_8
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_30
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_30
    add
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
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_31
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_31
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_7
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_7
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_9:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_32
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_32
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
    push 64
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_154
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_179
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_172
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_153
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_9
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_11:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_12
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_12
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_33
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_33
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_12:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_13
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_13
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_34
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_34
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_11
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_11
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_13:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_14
    push 7
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_55
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_97
    push 7
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_97
    push 7
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_76
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_12
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_14:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_15
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_15
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_35
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_35
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_13
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_13
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_15:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_16
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_16
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_36
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_36
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_14
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_14
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_16:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_17
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_17
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_37
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_37
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_15
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_15
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_17:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_18
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_18
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_38
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_38
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_16
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_16
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_18:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_19
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_19
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_39
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_39
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_17
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_17
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_19:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_20
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_20
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_40
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_40
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_18
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_18
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_20:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_21
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_21
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_41
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_41
    divide
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_19
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_19
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_21:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_22
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_22
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_42
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_42
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_20
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_20
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_22:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_23
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_23
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_43
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_43
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_21
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_21
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_23:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_24
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_24
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_22
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_22
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_24:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_25
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_25
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_45
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_45
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_23
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_23
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_25:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_26
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_26
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_24
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_24
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_26:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_47
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_47
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_25
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_25
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_27:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_3
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_3
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_48
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_48
    mod
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_2
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_2
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_2
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_2
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_28:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_29
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_49
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_90
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_90
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_4
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_4
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_5
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_5
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_29:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_30
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_30
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_49
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_49
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_28
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_28
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_6
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_6
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_30:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_31
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_31
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_50
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_50
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_29
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_29
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_7
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_7
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_31:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_32
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_32
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_51
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_51
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_30
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_30
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_8
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_8
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_32:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_52
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_52
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_31
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_31
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_9
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_9
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_33:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_34
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_34
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_53
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_53
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_11
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_11
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_34:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_13
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_13
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_54
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_54
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_33
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_33
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_12
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_12
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_35:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_36
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_36
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_55
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_55
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_13
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_13
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_14
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_14
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_36:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_37
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_37
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_56
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_56
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_35
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_35
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_15
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_15
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_37:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_38
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_38
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_57
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_57
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_36
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_36
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_16
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_16
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_38:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_39
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_39
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_58
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_58
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_37
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_37
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_17
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_17
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_39:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_40
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_40
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_59
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_59
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_38
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_38
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_18
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_18
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_40:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_41
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_41
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_60
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_60
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_39
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_39
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_19
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_19
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_41:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_42
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_42
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_61
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_61
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_40
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_40
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_20
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_20
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_42:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_43
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_43
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_62
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_62
    in (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_41
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_41
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_21
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_21
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_43:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_42
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_42
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_22
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_22
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_45:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_64
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_64
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_24
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_24
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_47:
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_26
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_26
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_48:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_3
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_3
    mod
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_2
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_2
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_27
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_27
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_49:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_50
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_50
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_71
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_71
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_28
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_28
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_29
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_29
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_50:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_51
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_51
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_72
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_72
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_49
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_49
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_30
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_30
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_51:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_52
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_52
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_73
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_73
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_50
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_50
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_31
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_31
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_52:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_74
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_74
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_51
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_51
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_32
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_32
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_53:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_54
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_54
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_75
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_75
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_33
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_33
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_54:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_13
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_13
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_76
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_76
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_53
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_53
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_34
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_34
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_55:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_56
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_56
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_78
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_78
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_13
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_13
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_35
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_35
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_56:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_57
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_57
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_79
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_79
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_55
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_55
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_36
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_36
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_57:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_58
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_58
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_80
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_80
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_56
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_56
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_37
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_37
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_58:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_59
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_59
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_81
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_81
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_57
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_57
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_38
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_38
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_59:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_60
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_60
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_82
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_82
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_58
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_58
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_39
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_39
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_60:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_61
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_61
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_83
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_83
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_59
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_59
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_40
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_40
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_61:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_62
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_62
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_84
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_84
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_60
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_60
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_41
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_41
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_62:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_85
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_85
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_61
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_61
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_42
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_42
    add
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_66:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_1
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_1
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_2
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_2
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_68:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_69
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_69
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_4
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_4
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_69:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_71
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_71
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_89
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_89
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_68
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_68
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_4
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_4
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_71:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_72
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_72
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_91
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_91
    out (char)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_69
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_69
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_49
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_49
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_72:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_73
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_73
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_92
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_92
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_71
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_71
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_50
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_50
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_73:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_74
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_74
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_93
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_93
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_72
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_72
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_51
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_51
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_74:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_94
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_94
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_73
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_73
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_52
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_52
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_75:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_76
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_76
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_95
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_95
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_53
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_53
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_76:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_13
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_13
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_96
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_96
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_75
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_75
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_54
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_54
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_78:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_79
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_79
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_99
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_99
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_55
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_55
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_79:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_80
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_80
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_100
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_100
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_78
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_78
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_56
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_56
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_80:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_81
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_81
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_101
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_101
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_79
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_79
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_57
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_57
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_81:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_82
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_82
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_102
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_102
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_80
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_80
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_58
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_58
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_82:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_83
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_83
    in (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_103
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_103
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_81
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_81
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_59
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_59
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_83:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_84
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_84
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_104
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_104
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_82
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_82
    mod
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_60
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_60
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_84:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_85
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_85
    in (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_105
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_105
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_83
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_83
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_61
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_61
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_85:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_106
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_106
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_84
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_84
    add
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_62
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_62
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_89:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_90
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_90
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_114
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_114
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_69
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_69
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_90:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_91
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_91
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_115
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_115
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_89
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_89
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_28
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_28
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_91:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_92
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_92
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_116
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_116
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_90
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_90
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_71
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_71
    subtract
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_92:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_93
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_93
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_117
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_117
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_91
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_91
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_72
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_72
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_93:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_94
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_94
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_118
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_118
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_92
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_92
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_73
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_73
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_94:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_119
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_119
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_93
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_93
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_74
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_74
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_95:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_96
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_96
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_120
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_120
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_75
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_75
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_96:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_97
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_97
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_121
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_121
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_95
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_95
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_76
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_76
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_97:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_98
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_98
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_96
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_96
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_13
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_13
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_98:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_99
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_99
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_123
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_123
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_97
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_97
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_99:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_100
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_100
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_124
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_124
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_98
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_98
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_78
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_78
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_100:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_101
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_101
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_125
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_125
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_99
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_99
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_79
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_79
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_101:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_102
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_102
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_126
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_126
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_100
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_100
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_80
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_80
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_102:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_103
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_103
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_127
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_127
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_101
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_101
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_81
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_81
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_103:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_104
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_104
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_128
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_128
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_102
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_102
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_82
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_82
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_104:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_105
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_105
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_103
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_103
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_83
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_83
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_105:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_106
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_106
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_104
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_104
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_84
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_84
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_106:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_107
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_107
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_105
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_105
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_85
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_85
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_107:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_108
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_108
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_106
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_106
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_108:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_107
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_107
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_114:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_115
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_115
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_130
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_130
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_89
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_89
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_115:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_116
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_116
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_131
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_131
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_114
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_114
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_90
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_90
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_116:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_117
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_117
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_132
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_132
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_115
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_115
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_91
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_91
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_117:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_118
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_118
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_133
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_133
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_116
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_116
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_92
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_92
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_118:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_119
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_119
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_134
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_134
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_117
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_117
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_93
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_93
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_119:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_135
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_135
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_118
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_118
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_94
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_94
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_120:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_121
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_121
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_136
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_136
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_95
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_95
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_121:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_122
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_122
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_137
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_137
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_120
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_120
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_96
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_96
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_122:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_123
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_123
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_138
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_138
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_121
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_121
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_97
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_97
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_123:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_124
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_124
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_139
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_139
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_122
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_122
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_98
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_98
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_124:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_125
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_125
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_140
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_140
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_123
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_123
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_99
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_99
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_125:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_126
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_126
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_141
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_141
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_124
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_124
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_100
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_100
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_126:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_127
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_127
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_142
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_142
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_125
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_125
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_101
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_101
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_127:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_128
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_128
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_143
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_143
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_126
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_126
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_102
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_102
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_128:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_144
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_144
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_127
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_127
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_103
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_103
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_130:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_131
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_131
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_148
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_148
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_114
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_114
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_131:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_132
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_132
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_149
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_149
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_130
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_130
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_115
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_115
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_132:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_133
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_133
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_150
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_150
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_131
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_131
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_116
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_116
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_133:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_134
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_134
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_151
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_151
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_132
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_132
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_117
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_117
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_134:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_135
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_135
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_152
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_152
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_133
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_133
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_118
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_118
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_135:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_153
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_153
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_134
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_134
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_119
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_119
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_136:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_137
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_137
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_154
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_154
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_120
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_120
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_137:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_138
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_138
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_155
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_155
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_136
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_136
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_121
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_121
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_138:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_139
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_139
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_156
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_156
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_137
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_137
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_122
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_122
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_139:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_140
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_140
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_157
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_157
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_138
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_138
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_123
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_123
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_140:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_141
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_141
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_158
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_158
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_139
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_139
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_124
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_124
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_141:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_142
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_142
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_159
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_159
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_140
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_140
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_125
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_125
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_142:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_143
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_143
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_160
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_160
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_141
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_141
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_126
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_126
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_143:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_144
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_144
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_161
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_161
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_142
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_142
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_127
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_127
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_144:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_162
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_162
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_143
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_143
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_128
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_128
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_148:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_149
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_149
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_166
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_166
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_130
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_130
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_149:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_150
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_150
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_167
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_167
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_148
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_148
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_131
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_131
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_150:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_151
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_151
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_168
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_168
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_149
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_149
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_132
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_132
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_151:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_152
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_152
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_169
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_169
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_150
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_150
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_133
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_133
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_152:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_153
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_153
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_170
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_170
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_151
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_151
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_134
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_134
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_153:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_10
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_10
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_171
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_171
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_152
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_152
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_135
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_135
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_154:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_155
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_155
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_180
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_180
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_10
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_10
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_136
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_136
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_155:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_156
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_156
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_181
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_181
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_154
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_154
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_137
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_137
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_156:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_157
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_157
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_182
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_182
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_155
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_155
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_138
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_138
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_157:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_158
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_158
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_183
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_183
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_156
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_156
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_139
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_139
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_158:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_159
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_159
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_184
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_184
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_157
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_157
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_140
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_140
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_159:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_160
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_160
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_185
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_185
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_158
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_158
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_141
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_141
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_160:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_161
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_161
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_186
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_186
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_159
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_159
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_142
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_142
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_161:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_162
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_162
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_187
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_187
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_160
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_160
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_143
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_143
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_162:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_163
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_163
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_188
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_188
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_161
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_161
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_144
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_144
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_163:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_189
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_189
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_162
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_162
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_166:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_167
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_167
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_194
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_194
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_148
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_148
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_167:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_168
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_168
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_195
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_195
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_166
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_166
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_149
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_149
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_168:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_169
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_169
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_196
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_196
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_167
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_167
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_150
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_150
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_169:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_170
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_170
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_197
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_197
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_168
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_168
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_151
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_151
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_170:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_171
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_171
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_198
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_198
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_169
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_169
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_152
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_152
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_171:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_172
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_172
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_199
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_199
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_170
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_170
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_153
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_153
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_172:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_173
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_173
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_200
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_200
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_171
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_171
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_10
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_10
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_173:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_174
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_174
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_201
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_201
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_172
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_172
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_10
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_10
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_174:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_175
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_175
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_202
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_202
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_173
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_173
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_10
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_10
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_175:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_176
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_176
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_203
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_203
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_174
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_174
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_10
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_10
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_176:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_177
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_177
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_204
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_204
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_175
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_175
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_10
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_10
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_177:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_178
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_178
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_205
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_205
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_176
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_176
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_10
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_10
    duplicate
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_178:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_179
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_179
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_206
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_206
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_177
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_177
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_10
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_10
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_179:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_180
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_180
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_207
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_207
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_178
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_178
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_10
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_10
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_180:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_181
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_181
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_208
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_208
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_179
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_179
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_154
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_154
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_181:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_182
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_182
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_209
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_209
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_180
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_180
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_155
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_155
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_182:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_183
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_183
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_210
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_210
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_181
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_181
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_156
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_156
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_183:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_184
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_184
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_211
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_211
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_182
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_182
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_157
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_157
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_184:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_185
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_185
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_212
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_212
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_183
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_183
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_158
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_158
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_185:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_186
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_186
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_213
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_213
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_184
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_184
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_159
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_159
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_186:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_187
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_187
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_214
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_214
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_185
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_185
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_160
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_160
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_187:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_188
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_188
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_215
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_215
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_186
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_186
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_161
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_161
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_188:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_189
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_189
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_216
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_216
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_187
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_187
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_162
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_162
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_189:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_188
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_188
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_163
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_163
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_194:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_195
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_195
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_221
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_221
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_166
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_166
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_195:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_196
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_196
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_222
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_222
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_194
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_194
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_167
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_167
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_196:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_197
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_197
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_223
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_223
    not
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_195
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_195
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_168
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_168
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_197:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_198
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_198
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_224
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_224
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_196
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_196
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_169
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_169
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_198:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_199
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_199
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_225
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_225
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_197
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_197
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_170
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_170
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_199:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_200
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_200
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_226
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_226
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_198
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_198
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_171
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_171
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_200:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_201
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_201
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_227
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_227
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_199
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_199
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_172
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_172
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_201:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_202
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_202
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_228
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_228
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_200
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_200
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_173
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_173
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_202:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_203
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_203
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_229
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_229
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_201
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_201
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_174
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_174
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_203:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_204
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_204
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_230
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_230
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_202
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_202
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_175
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_175
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_204:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_205
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_205
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_231
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_231
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_203
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_203
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_176
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_176
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_205:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_206
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_206
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_232
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_232
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_204
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_204
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_177
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_177
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_206:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_207
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_207
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_233
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_233
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_205
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_205
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_178
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_178
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_207:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_208
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_208
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_234
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_234
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_206
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_206
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_179
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_179
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_208:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_209
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_209
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_235
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_235
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_207
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_207
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_180
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_180
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_209:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_210
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_210
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_236
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_236
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_208
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_208
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_181
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_181
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_210:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_211
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_211
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_237
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_237
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_209
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_209
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_182
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_182
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_211:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_212
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_212
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_238
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_238
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_210
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_210
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_183
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_183
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_212:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_213
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_213
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_239
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_239
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_211
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_211
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_184
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_184
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_213:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_214
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_214
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_240
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_240
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_212
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_212
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_185
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_185
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_214:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_215
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_215
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_241
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_241
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_213
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_213
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_186
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_186
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_215:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_216
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_216
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_242
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_242
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_214
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_214
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_187
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_187
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_216:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_243
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_243
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_215
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_215
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_188
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_188
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_221:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_222
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_222
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_247
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_247
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_194
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_194
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_222:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_223
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_223
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_248
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_248
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_221
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_221
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_195
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_195
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_223:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_224
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_224
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_249
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_249
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_222
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_222
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_196
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_196
    roll
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_224:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_225
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_225
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_250
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_250
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_223
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_223
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_197
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_197
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_225:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_226
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_226
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_251
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_251
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_224
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_224
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_198
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_198
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_226:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_227
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_227
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_252
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_252
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_225
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_225
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_199
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_199
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_227:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_228
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_228
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_253
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_253
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_226
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_226
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_200
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_200
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_228:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_229
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_229
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_254
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_254
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_227
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_227
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_201
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_201
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_229:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_230
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_230
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_255
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_255
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_228
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_228
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_202
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_202
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_230:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_231
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_231
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_256
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_256
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_229
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_229
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_203
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_203
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_231:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_232
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_232
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_257
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_257
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_230
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_230
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_204
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_204
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_232:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_233
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_233
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_258
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_258
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_231
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_231
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_205
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_205
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_233:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_234
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_234
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_259
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_259
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_232
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_232
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_206
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_206
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_234:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_235
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_235
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_260
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_260
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_233
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_233
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_207
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_207
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_235:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_236
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_236
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_261
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_261
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_234
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_234
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_208
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_208
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_236:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_237
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_237
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_262
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_262
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_235
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_235
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_209
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_209
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_237:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_238
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_238
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_263
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_263
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_236
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_236
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_210
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_210
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_238:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_239
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_239
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_264
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_264
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_237
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_237
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_211
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_211
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_239:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_240
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_240
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_265
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_265
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_238
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_238
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_212
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_212
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_240:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_241
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_241
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_266
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_266
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_239
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_239
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_213
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_213
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_241:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_242
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_242
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_267
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_267
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_240
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_240
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_214
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_214
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_242:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_243
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_243
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_268
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_268
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_241
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_241
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_215
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_215
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_243:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_269
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_269
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_242
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_242
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_216
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_216
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_247:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_248
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_248
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_221
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_221
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_248:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_249
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_249
    greater
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_247
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_247
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_222
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_222
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_249:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_250
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_250
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_248
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_248
    greater
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_223
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_223
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_250:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_251
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_251
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_249
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_249
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_224
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_224
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_251:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_252
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_252
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_250
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_250
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_225
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_225
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_252:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_253
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_253
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_251
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_251
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_226
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_226
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_253:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_254
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_254
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_252
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_252
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_227
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_227
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_254:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_255
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_255
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_253
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_253
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_228
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_228
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_255:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_256
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_256
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_254
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_254
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_229
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_229
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_256:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_257
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_257
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_255
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_255
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_230
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_230
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_257:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_258
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_258
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_256
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_256
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_231
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_231
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_258:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_259
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_259
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_257
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_257
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_232
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_232
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_259:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_260
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_260
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_258
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_258
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_233
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_233
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_260:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_261
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_261
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_259
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_259
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_234
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_234
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_261:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_262
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_262
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_260
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_260
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_235
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_235
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_262:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_263
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_263
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_261
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_261
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_236
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_236
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_263:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_264
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_264
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_262
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_262
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_237
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_237
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_264:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_265
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_265
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_263
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_263
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_238
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_238
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_265:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_266
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_266
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_264
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_264
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_239
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_239
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_266:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_267
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_267
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_265
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_265
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_240
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_240
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_267:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_268
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_268
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_266
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_266
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_241
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_241
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_268:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_269
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_269
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_267
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_267
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_242
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_242
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_269:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_268
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_268
    push 1
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_243
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_243
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
