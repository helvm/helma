; --- PIET FLAT ASSEMBLY LISTING ---
; Entry point: label_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

label_0:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_1
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_1
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_1:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_2
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_2
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_2:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_3
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_3
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_51
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_51
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_1
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_3:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_65
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_65
    multiply
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_51
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_2
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_5:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_6
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_65
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_65
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_6:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_5
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_5
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_8:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_36
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_6
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_11:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_12
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_12
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_12:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_40
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_11
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_11
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_14:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_15
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_15
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_40
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_15:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_41
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_14
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_14
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_17:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_18
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_18
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_42
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_18:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_43
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_17
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_17
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_20:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_39
    push 14
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_39
    push 14
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_43
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_33:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_42
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_41
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_36:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_37
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_37
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_8
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_8
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_37:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_38
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_38
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_36
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_36
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_38:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_11
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_11
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_37
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_37
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_39:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_44
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_20
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_20
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_40:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_14
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_14
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_12
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_12
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_41:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_33
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_33
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_15
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_15
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_42:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_17
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_17
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_33
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_33
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_43:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_20
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_20
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_18
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_18
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_44:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_45
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_45
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_39
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_39
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_45:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_46
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_46
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_44
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_44
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_46:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_50
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_50
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_45
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_45
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_50:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_62
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_62
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_46
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_46
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_51:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_3
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_3
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_65
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_65
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_2
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_2
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_62:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_66
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_66
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_50
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_50
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_63:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_64
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_69
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_69
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_64:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_65
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_65
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_63
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_63
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_65:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_66
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_66
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_65
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_65
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_64
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_64
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_51
    in (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_3
    out (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_66:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_65
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_65
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_65
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_65
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_62
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_62
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_69:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_70
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_70
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_63
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_63
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_70:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_71
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_71
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_102
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_102
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_69
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_69
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_71:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_92
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_104
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_103
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_70
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_70
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_72:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_93
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_93
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_108
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_108
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_92
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_74:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_75
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_75
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_109
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_109
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_93
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_74
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_75:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_94
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_109
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_109
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_74
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_74
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_76:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_95
    push 19
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_110
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_94
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_85:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_86
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_86
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_95
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_86:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_96
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_85
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_85
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_87:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_88
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_88
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_112
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_112
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_96
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_88:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_97
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_112
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_112
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_87
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_87
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_89:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_90
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_90
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_113
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_113
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_97
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_90:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_98
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_113
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_113
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_89
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_89
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_92:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_72
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_72
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_107
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_107
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_71
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_71
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_93:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_74
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_74
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_108
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_108
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_72
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_72
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_74
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_74
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_94:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_76
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_76
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_109
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_109
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_75
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_75
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_95:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_85
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_85
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_111
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_76
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_76
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_96:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_87
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_87
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_111
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_111
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_86
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_86
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_97:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_89
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_89
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_113
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_113
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_88
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_88
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_98:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_99
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_99
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_113
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_113
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_90
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_90
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_99:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_100
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_100
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_113
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_113
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_98
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_98
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_100:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_101
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_101
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_99
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_99
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_101:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_105
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_105
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_70
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_70
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_100
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_100
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_102:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_103
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_114
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_114
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_70
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_70
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_103:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_104
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_104
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_102
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_102
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_71
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_71
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_104:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_105
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_105
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_107
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_107
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_103
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_103
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_71
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_71
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_105:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_113
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_113
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_104
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_104
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_101
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_101
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_107:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_108
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_108
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_104
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_104
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_108:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_109
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_109
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_107
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_107
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_72
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_72
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_109:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_110
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_110
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_108
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_108
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_74
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_75
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_110:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_111
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_109
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_76
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_76
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_111:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_112
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_112
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_110
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_110
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_76
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_86
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_112:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_113
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_111
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_87
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_87
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_113:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_113
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_112
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_112
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_97
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_90
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_114:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_102
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_102
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_122:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_123
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_123
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_114
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_114
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_123:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_134
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_122
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_122
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_124:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_135
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_135
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_134
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_126:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_127
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_127
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_135
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_126
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_127:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_136
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_126
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_126
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_128:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_129
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_129
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_137
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_129:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_138
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_148
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_128
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_128
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_130:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_131
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_131
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_156
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_156
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_139
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_131:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_140
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_158
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_156
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_130
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_130
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_132:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_137
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_136
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_133:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_139
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_150
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_150
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_138
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_134:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_124
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_124
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_123
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_123
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_135:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_126
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_126
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_124
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_124
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_126
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_126
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_136:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_132
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_132
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_127
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_127
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_137:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_128
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_128
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_132
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_132
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_138:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_133
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_133
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_149
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_149
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_129
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_129
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_139:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_130
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_130
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_150
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_150
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_133
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_133
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_140:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_141
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_141
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_158
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_158
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_131
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_131
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_141:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_142
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_142
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_158
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_158
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_140
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_140
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_142:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_143
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_143
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_159
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_159
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_141
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_141
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_143:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_144
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_144
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_160
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_160
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_142
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_142
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_144:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_145
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_145
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_160
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_160
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_143
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_143
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_145:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_147
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_147
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_144
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_144
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_147:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_151
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_151
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_145
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_145
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_148:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_149
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_170
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_170
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_122
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_129
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_129
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_149:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_150
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_150
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_148
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_148
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_138
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_138
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_150:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_151
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_151
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_155
    duplicate
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_150
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_149
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_149
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_133
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_139
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_151:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_160
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_160
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_150
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_150
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_147
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_147
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_155:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_156
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_156
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_150
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_150
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_150
    divide
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_156:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_157
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_155
    push 5
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_155
    push 5
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_130
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_130
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_157:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_158
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_158
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_156
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_131
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_131
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_158:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_159
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_157
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_157
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_131
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_140
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_159:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_160
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_160
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_158
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_142
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_142
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_160:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_160
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_159
    push 20
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_159
    push 20
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_143
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
label_161:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_162
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_162
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_201
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_201
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_162:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_163
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_202
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_202
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_161
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_161
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_163:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_165
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_203
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_203
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_162
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_165:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_166
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_166
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_205
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_205
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_163
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_165
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_166:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_167
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_205
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_205
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_165
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_165
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_167:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_168
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_206
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_206
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_166
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_168:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_169
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_169
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_207
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_207
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_167
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_169:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_176
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_208
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_208
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_168
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_168
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_170:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_176
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_176
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_148
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_148
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_171:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_172
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_178
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_187
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_187
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_172:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_174
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_174
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_178
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_178
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_171
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_171
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_174:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_175
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_182
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_197
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_197
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_181
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_172
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_175:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_184
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_182
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_174
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_174
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_176:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_177
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_177
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_209
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_209
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_169
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_170
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_170
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_177:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_188
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_210
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_210
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_176
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_176
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_178:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_179
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_179
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_194
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_194
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_187
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_171
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_172
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_172
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_179:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_180
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_180
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_195
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_195
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_178
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_178
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_174
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_174
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_180:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_181
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_181
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_196
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_196
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_179
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_179
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_174
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_174
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_181:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_174
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_174
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_197
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_197
    subtract
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_180
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_180
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_174
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_174
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_182:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_183
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_183
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_197
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_197
    out (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_174
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_174
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_175
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_175
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_183:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_184
    subtract
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_185
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_198
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_198
    in (number)
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_182
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_182
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_175
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_175
    in (number)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_184:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_185
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_185
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_183
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_183
    out (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_175
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_175
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_185:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_198
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_183
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_184
    push 3
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_184
    push 3
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_186:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_191
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_214
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_212
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_190
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
label_187:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_178
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_178
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_193
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_193
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_171
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_171
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_188:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_189
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_189
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_210
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_210
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_177
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_177
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_189:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_190
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_190
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_212
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_211
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_188
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_188
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_190:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_186
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_186
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_212
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_212
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_189
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_189
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_191:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_192
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_192
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_214
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_214
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_186
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_186
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_192:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_193
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_193
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_214
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_214
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_191
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_191
    in (char)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_193:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_194
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_194
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_192
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_192
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_187
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_187
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_194:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_195
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_195
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_193
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_193
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_178
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_178
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_195:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_196
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_196
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_194
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_194
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_179
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_179
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_196:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_197
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_197
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_200
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_200
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_195
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_195
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_180
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_180
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_197:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_198
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_198
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_196
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_196
    subtract
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_181
    out (char)
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_182
    multiply
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_198:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_185
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_185
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_197
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_197
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_183
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_183
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_200:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_217
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_217
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_196
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_196
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_201:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_202
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_202
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_161
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_161
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_202:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_203
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_201
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_201
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_162
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_162
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_203:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_204
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_202
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_163
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_163
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_204:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_205
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_205
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_203
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_165
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_165
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_205:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_206
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_204
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_204
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_165
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_166
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_206:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_207
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_167
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_205
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_167
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_167
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_207:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_208
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_208
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_206
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_168
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_168
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_208:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_209
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_207
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_207
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_169
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_169
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_209:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_210
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_210
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_208
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_176
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_176
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_210:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_211
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_209
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_209
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_177
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_177
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_211:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_212
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_212
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_210
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_189
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_189
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_212:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_213
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_211
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_211
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_189
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_190
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_213:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_214
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_214
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_212
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_186
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_186
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_214:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_215
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_213
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_213
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_186
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_191
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
label_215:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_216
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_216
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_214
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_193
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_193
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
label_216:
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_215
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_215
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_193
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
