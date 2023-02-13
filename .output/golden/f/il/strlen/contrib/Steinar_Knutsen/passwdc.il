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
]