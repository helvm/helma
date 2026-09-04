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
block_1:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_2
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
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 10
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_51
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_51
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_1
block_3:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_5
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_51
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_2
block_5:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_6
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_3
block_6:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_5
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_5
block_8:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_36
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_6
block_11:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_12
block_12:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_40
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_11
block_14:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_40
block_15:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_41
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_14
block_17:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_42
block_18:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_43
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_17
block_20:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 14
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 14
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_39
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_43
block_33:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_42
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_41
block_36:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_37
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_8
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_8
block_37:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_38
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_38
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_36
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_36
block_38:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_11
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_11
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_37
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_37
block_39:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_20
block_40:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_14
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_14
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_12
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_12
block_41:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_15
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_15
block_42:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_17
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_17
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_33
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_33
block_43:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_20
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_20
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_18
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_18
block_44:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_45
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_45
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_39
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_39
block_45:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_46
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_46
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_44
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_44
block_46:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_50
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_50
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_45
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_45
block_50:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_62
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_62
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_46
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_46
block_51:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_3
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_3
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_2
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_2
block_62:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_66
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_50
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_50
block_63:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_69
block_64:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_63
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_63
block_65:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_66
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_66
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_64
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_64
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_51
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_3
block_66:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_65
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_65
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_62
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_62
block_69:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_70
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_63
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_63
block_70:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_71
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_102
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_102
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_69
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_69
block_71:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_92
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_103
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_70
block_72:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_93
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_93
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_108
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_108
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_92
block_74:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_75
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_75
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_109
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_109
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_93
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_74
block_75:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_94
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_109
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_109
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_74
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_74
block_76:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 19
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_95
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_110
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_94
block_85:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_86
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_111
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_95
block_86:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_96
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_111
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_85
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_85
block_87:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_88
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_88
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_112
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_112
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_96
block_88:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_97
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_112
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_112
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_87
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_87
block_89:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_90
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_90
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_97
block_90:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_98
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_89
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_89
block_92:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_72
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_107
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_107
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_71
block_93:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_74
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_74
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_108
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_108
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_72
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_74
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_74
block_94:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_76
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_76
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_109
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_109
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_75
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_75
block_95:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_85
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_85
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_111
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_76
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_76
block_96:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_87
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_87
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_111
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_86
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_86
block_97:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_89
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_89
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_88
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_88
block_98:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_99
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_99
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_113
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_90
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_90
block_99:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_100
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_100
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_113
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_98
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_98
block_100:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_101
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_101
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_99
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_99
block_101:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_105
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_105
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_70
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_100
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_100
block_102:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_103
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_114
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_114
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_70
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_70
block_103:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_104
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_102
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_102
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_71
block_104:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_105
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_105
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_107
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_107
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_103
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_103
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_71
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_71
block_105:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_113
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_104
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_101
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_101
block_107:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_108
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_108
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_104
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    greater
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_104
block_108:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_109
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_109
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_107
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_107
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_72
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_72
block_109:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_110
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_110
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_108
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_108
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_74
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_75
block_110:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_111
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_109
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_76
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_76
block_111:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_112
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_112
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_110
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_110
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_76
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_86
block_112:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_111
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_87
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_87
block_113:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_113
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_112
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_112
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_97
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_90
block_114:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_102
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_102
block_122:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_123
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_123
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_114
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_114
block_123:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_134
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_122
block_124:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_135
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 11
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_135
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_134
block_126:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_127
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_127
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_135
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_126
block_127:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_136
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_126
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_126
block_128:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_129
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_129
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_137
block_129:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_138
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_148
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_128
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_128
block_130:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_131
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_131
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_156
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_156
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_139
block_131:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_140
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_158
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_156
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_130
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_130
block_132:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_137
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_136
block_133:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_139
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_150
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_138
block_134:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_124
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_124
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_123
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_123
block_135:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_126
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_126
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_124
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_124
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_126
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_126
block_136:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_132
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_132
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_127
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_127
block_137:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_128
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_128
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_132
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_132
block_138:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_133
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_133
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_149
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_149
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_129
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_129
block_139:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_130
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_130
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_150
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_133
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_133
block_140:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_141
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_141
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_158
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_158
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_131
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_131
block_141:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_142
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_142
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_158
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_158
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_140
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_140
block_142:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_143
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_143
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_159
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_159
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_141
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_141
block_143:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_144
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_144
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_160
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_160
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_142
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_142
block_144:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_145
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_145
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_160
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_160
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_143
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_143
block_145:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_147
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_147
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_144
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_144
block_147:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_151
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_151
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_122
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_145
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_145
block_148:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_149
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_170
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_170
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_122
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_129
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_129
block_149:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_150
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_148
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_148
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_138
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_138
block_150:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_151
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_151
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_155
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_150
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_149
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_149
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_133
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_139
block_151:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_160
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_160
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_150
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_147
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_147
block_155:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_156
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_156
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_150
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_150
block_156:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_157
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 5
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_155
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 5
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_155
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_130
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_130
block_157:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_158
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_158
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_156
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_131
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_131
block_158:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_159
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_157
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 12
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_157
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_131
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_140
block_159:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_160
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_160
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_158
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_142
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_142
block_160:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_160
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 20
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_159
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 20
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_159
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_143
block_161:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_162
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_162
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_201
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_201
block_162:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_163
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_202
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_202
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_161
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_161
block_163:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_165
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_203
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_203
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_162
block_165:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_166
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_166
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_205
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_205
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_163
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_165
block_166:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_167
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_205
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_205
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_165
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_165
block_167:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_168
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_206
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_206
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_166
block_168:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_169
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 6
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_169
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_207
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_207
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_167
block_169:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_176
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_208
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_208
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_168
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_168
block_170:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_176
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_176
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_148
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_148
block_171:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_172
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_178
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_187
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_187
block_172:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_174
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_174
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_178
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_178
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_171
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_171
block_174:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_175
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_182
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_197
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_197
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_181
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_172
block_175:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_184
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_182
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_174
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_174
block_176:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_177
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 4
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_177
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_209
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_209
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_169
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_170
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_170
block_177:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_188
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_210
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_210
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_176
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_176
block_178:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_179
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_179
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_194
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_194
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_187
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_171
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_172
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_172
block_179:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_180
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_180
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_195
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_195
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_178
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_178
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_174
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_174
block_180:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_181
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_181
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_196
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_196
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_179
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_179
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_174
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_174
block_181:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_174
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_174
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_197
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_197
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_180
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_180
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_174
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_174
block_182:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_183
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_183
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_197
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_197
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_174
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_174
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_175
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_175
block_183:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_184
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_185
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_198
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_198
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_182
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_182
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_175
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    in_num
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_175
block_184:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_185
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_185
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_183
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_183
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_175
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_175
block_185:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_198
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_183
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_184
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_184
block_186:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 8
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_191
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_212
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_190
block_187:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_178
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_178
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_193
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_193
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_171
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_171
block_188:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_189
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_189
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_210
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_210
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_177
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_177
block_189:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_190
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_190
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_212
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_211
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_188
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    out_num
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_188
block_190:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_186
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_186
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_212
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_212
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_189
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_189
block_191:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_192
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_192
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_214
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_186
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_186
block_192:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_193
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_193
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_214
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_191
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    in_char
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_191
block_193:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_194
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    dup
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_194
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_192
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_192
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_187
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_187
block_194:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_195
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    not
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_195
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_193
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    div
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_193
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_178
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_178
block_195:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_196
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_196
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_194
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    roll
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_194
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_179
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_179
block_196:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_197
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    out_char
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_197
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_200
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    push 1
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_200
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_195
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    switch
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_195
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_180
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_180
block_197:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_198
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_198
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_196
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    sub
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_196
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    out_char
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_181
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mul
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_182
block_198:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_185
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    pointer
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_185
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_197
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_197
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_183
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    mod
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_183
block_200:
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCLeft}
    jump block_217
  case_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}:
    add
    set_dpcc Course {directionPointer = DPDown, codelChooser = CCRight}
    jump block_217
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_196
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_196
block_201:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_202
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_202
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_161
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_161
block_202:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_203
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_201
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_201
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_162
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_162
block_203:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_204
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_202
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_163
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_163
block_204:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_205
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_205
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_203
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_165
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_165
block_205:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_206
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_204
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_204
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_165
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_166
block_206:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_207
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_205
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_167
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_167
block_207:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_208
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_208
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_206
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_168
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_168
block_208:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_209
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_207
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_207
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_169
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_169
block_209:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_210
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_210
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_208
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_176
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_176
block_210:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_211
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_209
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_209
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_177
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_177
block_211:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_212
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_212
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_210
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_189
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_189
block_212:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_213
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_211
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_211
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_189
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_190
block_213:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_214
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 2
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_214
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_212
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_186
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_186
block_214:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_213
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_213
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}
    jump block_186
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}
    jump block_191
block_215:
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_216
  case_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}:
    push 3
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_216
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_214
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_193
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCRight}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCLeft}
    jump block_193
block_216:
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCLeft}
    jump block_215
  case_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}:
    pop
    set_dpcc Course {directionPointer = DPLeft, codelChooser = CCRight}
    jump block_215
  case_dpcc Course {directionPointer = DPUp, codelChooser = CCLeft}:
    set_dpcc Course {directionPointer = DPRight, codelChooser = CCRight}
    jump block_193
block_217:
