[ Comment " convert binary executable code to 80386 False source "
, Str "["
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
            ( SPure ( Binary Add ) )
        )
    ]
, Lambda
    [ Inst
        ( ISM ( SIO OutputDec ) )
    , Str "`"
    ]
, While
, Inst
    ( ISM ( SPure Discard ) )
, Str "]"
]