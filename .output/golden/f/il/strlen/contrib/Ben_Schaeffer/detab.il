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
            ( Cons 9 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Inst ( IAL Discard )
        , Inst
            ( IAL
                ( Cons 32 )
            )
        , Str "   "
        ]
    , Cond
    , Inst
        ( IAL ( SIO OutputChar ) )
    ]
, While
]