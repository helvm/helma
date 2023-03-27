[ Comment "
  detab.f: replaces all <tab>'s from input file with 4 spaces.
  detab <inputfile [ >outputfile ]

  Ben Schaeffer
  "
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
                ( Cons 9 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Inst
            ( IAL ( SPure Discard ) )
        , Inst
            ( IAL
                ( SPure
                    ( Cons 32 )
                )
            )
        , Str "   "
        ]
    , Cond
    , Inst
        ( IAL ( SIO OutputChar ) )
    ]
, While
]