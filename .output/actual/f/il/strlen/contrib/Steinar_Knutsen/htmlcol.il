[ Comment "
  This one formats input and puts each line into a 4 columns wide row in an 
  HTML-table. (Nice for automatically generated web-pages.)

  Steinar Knutsen <sk@nvg.unit.no>
  "
, Str "<table>
  "
, Lambda
    [ Str "<td>"
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
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Lambda
            [ Inst
                ( ISM ( SPure Discard ) )
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
                        ( Cons 10 )
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
            ]
        , Cond
        ]
    , Lambda
        [ Inst
            ( ISM ( SIO OutputChar ) )
        ]
    , While
    , Inst
        ( ISM ( SPure Discard ) )
    , Str "</td>"
    ]
, Ref 15
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
    [ Str "<tr>
      <td>"
    , Inst
        ( ISM ( SIO OutputChar ) )
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
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Lambda
            [ Inst
                ( ISM ( SPure Discard ) )
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
                        ( Cons 10 )
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
            ]
        , Cond
        ]
    , Lambda
        [ Inst
            ( ISM ( SIO OutputChar ) )
        ]
    , While
    , Inst
        ( ISM ( SPure Discard ) )
    , Str "</td>"
    , Ref 15
    , Fetch
    , Exec
    , Ref 15
    , Fetch
    , Exec
    , Ref 15
    , Fetch
    , Exec
    , Str "
      "
    ]
, While
, Inst
    ( ISM ( SPure Discard ) )
, Str "</table>
  "
]