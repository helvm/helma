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
    ( ISM
        ( SPure ( Binary Add ) )
    )
, Inst
    ( ISM ( SIO OutputDec ) )
, Str "
  "
]