[ Comment "
  "Life" in FALSE

  input is for example:

  ..-.
  --..
  ....
  ...-
  "
, Ref 1
, Inst
    ( IAL
        ( Cons 4 )
    )
, Ref 6
, Store
, Ref 6
, Fetch
, Inst
    ( IAL
        ( Cons 1 )
    )
, Inst
    ( IAL ( Binary Add ) )
, Lambda
    [ Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Sub ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    ]
, Lambda
    [ Ref 6
    , Fetch
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Add ) )
    , Lambda
        [ Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        ]
    , Lambda
        [ Inst
            ( IAL ( SIO InputChar ) )
        , Inst
            ( IAL
                ( Cons 42 )
            )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Inst
            ( IAL
                ( SStatic 2 Move )
            )
        , Inst
            ( IAL
                ( SStatic 2 Move )
            )
        ]
    , While
    , Inst ( IAL Discard )
    , Inst
        ( IAL ( SIO InputChar ) )
    , Inst ( IAL Discard )
    ]
, While
, Inst ( IAL Discard )
, Ref 6
, Fetch
, Inst
    ( IAL
        ( SStatic 0 Copy )
    )
, Ref 25
, Store
, Inst
    ( IAL
        ( SStatic 0 Copy )
    )
, Inst
    ( IAL ( Binary Mul ) )
, Ref 4
, Store
, Lambda
    [ Ref 25
    , Fetch
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Sub ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Ref 25
    , Store
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