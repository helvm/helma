[ Comment "USE read"
, Str "a:
  "
, Ref 17
, Fetch
, Exec
, Str "b:
  "
, Ref 17
, Fetch
, Exec
, Inst
    ( IAL
        ( SAL ( Binary Add ) )
    )
, Inst
    ( IAL ( SIO OutputDec ) )
, Str "
  "
]