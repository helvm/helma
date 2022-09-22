[ Comment " addcr.f: convert amiga txt --> smdos txt.
    addcr <inputfile [ >outputfile ]                "
, Flush
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
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
            ( Cons 10 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Inst
            ( IAL
                ( Cons 13 )
            )
        , Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , Cond
    , Inst
        ( IAL ( SIO OutputChar ) )
    ]
, While
, Inst
    ( IAL
        ( Cons 26 )
    )
, Inst
    ( IAL ( SIO OutputChar ) )
]