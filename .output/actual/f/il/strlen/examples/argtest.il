[ Comment " this program tests the argument facility of the portable interpreter "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SAL
            ( Cons 2 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SAL ( Unary BNot ) )
    )
, Inst
    ( IAL
        ( SAL
            ( SStatic 0 Copy )
        )
    )
, Lambda
    [ Str "2 integer arguments expected
    " ]
, Cond
, Inst
    ( IAL
        ( SAL ( Unary BNot ) )
    )
, Lambda
    [ Ref 1
    , Fetch
    , Inst
        ( IAL ( SIO OutputDec ) )
    , Str " + "
    , Ref 2
    , Fetch
    , Inst
        ( IAL ( SIO OutputDec ) )
    , Str " = "
    , Ref 1
    , Fetch
    , Ref 2
    , Fetch
    , Inst
        ( IAL
            ( SAL ( Binary Add ) )
        )
    , Inst
        ( IAL ( SIO OutputDec ) )
    , Str "
      "
    ]
, Cond
]