[ Comment " this program tests the argument facility of the portable interpreter "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( Cons 2 )
    )
, Inst
    ( IAL ( Binary LEQ ) )
, Inst
    ( IAL ( Unary BNot ) )
, Inst
    ( IAL
        ( SStatic 0 Copy )
    )
, Lambda
    [ Str "2 integer arguments expected
    " ]
, Cond
, Inst
    ( IAL ( Unary BNot ) )
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
        ( IAL ( Binary Add ) )
    , Inst
        ( IAL ( SIO OutputDec ) )
    , Str "
      "
    ]
, Cond
]