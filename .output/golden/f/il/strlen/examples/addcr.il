[ Comment " addcr.f: convert amiga txt --> smdos txt.
    addcr <inputfile [ >outputfile ]                "
, Flush
, Lambda
    [ Inst
        ( ISM ( SIO InputChar ) )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Unary Neg ) )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( ISM
            ( SPure ( Unary BNot ) )
        )
    ]
, Lambda
    [ Inst
        ( ISM
            ( SPure
                ( Cons 10 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Cons 13 )
                )
            )
        , Inst
            ( ISM ( SIO OutputChar ) )
        ]
    , Cond
    , Inst
        ( ISM ( SIO OutputChar ) )
    ]
, While
, Inst
    ( ISM
        ( SPure
            ( Cons 26 )
        )
    )
, Inst
    ( ISM ( SIO OutputChar ) )
]