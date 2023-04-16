[ Comment "
  detab.f: replaces all <tab>'s from input file with 4 spaces.
  detab <inputfile [ >outputfile ]

  Ben Schaeffer
  "
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
                ( Cons 9 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Inst
            ( ISM ( SPure Discard ) )
        , Inst
            ( ISM
                ( SPure
                    ( Cons 32 )
                )
            )
        , Str "   "
        ]
    , Cond
    , Inst
        ( ISM ( SIO OutputChar ) )
    ]
, While
]