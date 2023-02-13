[ Comment "
  This changes passwd-files from UNIX to Amiga, that is, changes : as 
  delimiters to |, all home-directories with us are under /home/ and this 
  is changed to HOMES:

  Steinar Knutsen <sk@nvg.unit.no>
  "
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
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
]