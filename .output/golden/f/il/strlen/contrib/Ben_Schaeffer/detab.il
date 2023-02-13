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
                ( Cons 9 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Inst
            ( IAL ( SAL Discard ) )
        , Inst
            ( IAL
                ( SAL
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