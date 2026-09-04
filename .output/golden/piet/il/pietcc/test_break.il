; --- PIET FLAT ASSEMBLY LISTING ---
; Entry point: label_0
; Initial Course: Course {directionPointer = DPRight, codelChooser = CCLeft}

label_0:
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft} label_1
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPDown, codelChooser = CCRight} label_1
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
label_1:
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft} label_0
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    branch_dpcc Course {directionPointer = DPUp, codelChooser = CCRight} label_0
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
