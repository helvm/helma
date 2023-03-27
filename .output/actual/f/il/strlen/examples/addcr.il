[ Comment " addcr.f: convert amiga txt --> smdos txt.
    addcr <inputfile [ >outputfile ]                "
, Flush
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Unary Neg ) )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
        )
    ]
, Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Cons 10 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SPure
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
        ( SPure
            ( Cons 26 )
        )
    )
, Inst
    ( IAL ( SIO OutputChar ) )
]