[ Comment "chkbrack.f"
, Comment "checks brackets"
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 0
, Store
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 1
, Store
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 2
, Store
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
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
, Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 40 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 0
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Add ) )
            )
        , Ref 0
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 41 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 0
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Sub ) )
            )
        , Ref 0
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 91 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 1
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Add ) )
            )
        , Ref 1
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 93 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 1
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Sub ) )
            )
        , Ref 1
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 123 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Add ) )
            )
        , Ref 2
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 125 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Sub ) )
            )
        , Ref 2
        , Store
        ]
    , Cond
    ]
, While
, Str "#of () "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SPure ( Unary BNot ) )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Inst
    ( IAL
        ( SPure ( Unary BNot ) )
    )
, Lambda
    [ Str "is ok.
    " ]
, Cond
, Lambda
    [ Str "NOT ok !!!
    " ]
, Cond
, Str "#of [] "
, Ref 1
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SPure ( Unary BNot ) )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Inst
    ( IAL
        ( SPure ( Unary BNot ) )
    )
, Lambda
    [ Str "is ok.
    " ]
, Cond
, Lambda
    [ Str "NOT ok !!!
    " ]
, Cond
, Str "#of {} "
, Ref 2
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SPure ( Unary BNot ) )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Inst
    ( IAL
        ( SPure ( Unary BNot ) )
    )
, Lambda
    [ Str "is ok.
    " ]
, Cond
, Lambda
    [ Str "NOT OK !!!
    " ]
, Cond
]