[ Comment "split.f"
, Inst
    ( ISM ( SIO InputChar ) )
, Ref 0
, Store
, Lambda
    [ Ref 0
    , Fetch
    , Inst
        ( ISM
            ( SPure
                ( Cons 32 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    ]
, Lambda
    [ Inst
        ( ISM ( SIO InputChar ) )
    , Ref 0
    , Store
    ]
, While
, Comment " read operand for split: front/back "
, Ref 0
, Fetch
, Inst
    ( ISM
        ( SPure
            ( Cons 102 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary LEQ ) )
    )
, Ref 0
, Fetch
, Inst
    ( ISM
        ( SPure
            ( Cons 98 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( ISM
        ( SPure ( Binary BOr ) )
    )
, Inst
    ( ISM ( SIO InputChar ) )
, Inst
    ( ISM
        ( SPure
            ( Cons 32 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( ISM
        ( SPure ( Binary BAnd ) )
    )
, Inst
    ( ISM
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Comment " a has to be 'f' of 'b' followed by a space "
]