[ Num 0
, Num 1
, Num 0
, Var 2
, Ope Store
, Fn
    [ Num 10
    , Var 2
    , Ope Fetch
    , Ope Greater
    ]
, Fn
    [ Ope Dup
    , Ope Rot
    , Ope Plus
    , Ope Dup
    , Io WriteDec
    , Io
        ( Str " " )
    , Num 1
    , Var 2
    , Ope Fetch
    , Ope Plus
    , Var 2
    , Ope Store
    ]
, Ope While
, Io
    ( Str "
    " )
]