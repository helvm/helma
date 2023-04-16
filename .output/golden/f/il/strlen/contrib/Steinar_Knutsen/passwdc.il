[ Comment "
  This changes passwd-files from UNIX to Amiga, that is, changes : as 
  delimiters to |, all home-directories with us are under /home/ and this 
  is changed to HOMES:

  Steinar Knutsen <sk@nvg.unit.no>
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
]