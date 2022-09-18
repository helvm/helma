[ Comment "
  This changes passwd-files from UNIX to Amiga, that is, changes : as 
  delimiters to |, all home-directories with us are under /home/ and this 
  is changed to HOMES:

  Steinar Knutsen <sk@nvg.unit.no>
  "
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst ( IAL Dup )
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
]