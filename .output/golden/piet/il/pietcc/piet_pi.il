; --- PIET FLAT ASSEMBLY LISTING ---
; Entry point: label_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

label_0:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_32
    push 52
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_2:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_40
    push 8492
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_32:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_33
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_33
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_0
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_33:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_34
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_34
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_32
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_32
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_34:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_33
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_33
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_37:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_42
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_42
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_38:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_54
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_54
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
label_40:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_37
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_37
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_2
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_2
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_42:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_43
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_43
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_37
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_37
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_43:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_44
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_44
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_42
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_42
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_44:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_45
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_45
    duplicate
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_43
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_43
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_45:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_46
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_46
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_44
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_44
    divide
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_46:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_47
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_47
    multiply
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_45
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_45
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_47:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_48
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_48
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_46
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_46
    out (number)
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_48:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_49
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_49
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_47
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_47
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_49:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_50
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_50
    roll
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_48
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_48
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_50:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_51
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_51
    divide
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_49
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_49
    not
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_51:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_52
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_52
    out (number)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_50
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_50
    duplicate
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_52:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_38
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_38
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_51
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_51
    multiply
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
label_54:
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft} label_39
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPRight, codelChooser = CCRight} label_39
    out (char)
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft} label_38
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight} label_38
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
