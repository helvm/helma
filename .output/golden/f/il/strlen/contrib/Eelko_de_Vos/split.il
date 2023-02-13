[ Comment "split.f"
, Inst
    ( IAL ( SIO InputChar ) )
, Ref 0
, Store
, Lambda
    [ Ref 0
    , Fetch
    , Inst
        ( IAL
            ( SPure
                ( Cons 32 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    ]
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Ref 0
    , Store
    ]
, While
, Comment " read operand for split: front/back "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 102 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary LEQ ) )
    )
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 98 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SPure ( Binary BOr ) )
    )
, Inst
    ( IAL ( SIO InputChar ) )
, Inst
    ( IAL
        ( SPure
            ( Cons 32 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SPure ( Binary BAnd ) )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed Copy
                ( ImmediateO 0 )
            )
        )
    )
, Comment " a has to be 'f' of 'b' followed by a space "
]