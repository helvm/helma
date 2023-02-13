[ Comment " addcr.f: convert amiga txt --> smdos txt.
    addcr <inputfile [ >outputfile ]                "
, Flush
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Unary Neg ) )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
        )
    ]
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( Cons 10 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( Cons 13 )
                )
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
        ( SAL
            ( Cons 26 )
        )
    )
, Inst
    ( IAL ( SIO OutputChar ) )
]