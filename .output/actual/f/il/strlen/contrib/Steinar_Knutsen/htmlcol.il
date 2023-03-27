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
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Lambda
            [ Inst
                ( IAL ( SPure Discard ) )
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
                        ( Cons 10 )
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
            ]
        , Cond
        ]
    , Lambda
        [ Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , While
    , Inst
        ( IAL ( SPure Discard ) )
    , Str "</td>"
    ]
, Ref 15
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
    [ Str "<tr>
      <td>"
    , Inst
        ( IAL ( SIO OutputChar ) )
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
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Lambda
            [ Inst
                ( IAL ( SPure Discard ) )
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
                        ( Cons 10 )
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
            ]
        , Cond
        ]
    , Lambda
        [ Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , While
    , Inst
        ( IAL ( SPure Discard ) )
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
    ( IAL ( SPure Discard ) )
, Str "</table>
  "
]