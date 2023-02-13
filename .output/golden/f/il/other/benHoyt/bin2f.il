[ Comment " convert binary executable code to 80386 False source "
, Str "["
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
            ( SAL ( Binary Add ) )
        )
    ]
, Lambda
    [ Inst
        ( IAL ( SIO OutputDec ) )
    , Str "`"
    ]
, While
, Inst
    ( IAL ( SAL Discard ) )
, Str "]"
]