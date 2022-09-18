[ Comment " strip.f: removes all <cr>'s from input file (and keeps <lf>'s).
    strip <inputfile [ >outputfile ]                "
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst ( IAL Dup )
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Unary Neg ) )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda
    [ Inst
        ( IAL
            ( Cons 13 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    , Lambda
        [ Inst ( IAL Dup )
        , Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , Cond
    , Inst ( IAL Discard )
    ]
, While
]