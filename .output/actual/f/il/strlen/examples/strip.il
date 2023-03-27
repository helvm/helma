[ Comment " strip.f: removes all <cr>'s from input file (and keeps <lf>'s).
    strip <inputfile [ >outputfile ]                "
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
                ( Cons 13 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , Cond
    , Inst
        ( IAL ( SPure Discard ) )
    ]
, While
]