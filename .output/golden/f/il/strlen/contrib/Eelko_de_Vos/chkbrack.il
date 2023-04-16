[ Comment "chkbrack.f"
, Comment "checks brackets"
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 0
, Store
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 1
, Store
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 2
, Store
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
, Lambda
    [ Inst
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
                ( Cons 40 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 0
        , Fetch
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Add ) )
            )
        , Ref 0
        , Store
        ]
    , Cond
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
                ( Cons 41 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 0
        , Fetch
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Sub ) )
            )
        , Ref 0
        , Store
        ]
    , Cond
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
                ( Cons 91 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 1
        , Fetch
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Add ) )
            )
        , Ref 1
        , Store
        ]
    , Cond
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
                ( Cons 93 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 1
        , Fetch
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Sub ) )
            )
        , Ref 1
        , Store
        ]
    , Cond
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
                ( Cons 123 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Add ) )
            )
        , Ref 2
        , Store
        ]
    , Cond
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
                ( Cons 125 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
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
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( ISM
        ( SPure ( Unary BNot ) )
    )
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
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( ISM
        ( SPure ( Unary BNot ) )
    )
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
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( ISM
        ( SPure ( Unary BNot ) )
    )
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