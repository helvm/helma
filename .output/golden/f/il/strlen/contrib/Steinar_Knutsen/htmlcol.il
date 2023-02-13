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
                ( SAL ( Unary Neg ) )
            )
        , Inst
            ( IAL
                ( SAL ( Binary LEQ ) )
            )
        , Inst
            ( IAL
                ( SAL ( Unary BNot ) )
            )
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 0 Copy )
                )
            )
        , Lambda
            [ Inst
                ( IAL ( SAL Discard ) )
            , Inst
                ( IAL
                    ( SAL
                        ( SStatic 0 Copy )
                    )
                )
            , Inst
                ( IAL
                    ( SAL
                        ( Cons 10 )
                    )
                )
            , Inst
                ( IAL
                    ( SAL ( Binary LEQ ) )
                )
            , Inst
                ( IAL
                    ( SAL ( Unary BNot ) )
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
        ( IAL ( SAL Discard ) )
    , Str "</td>"
    ]
, Ref 15
, Store
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
            ( SAL ( Unary Neg ) )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
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
                ( SAL ( Unary Neg ) )
            )
        , Inst
            ( IAL
                ( SAL ( Binary LEQ ) )
            )
        , Inst
            ( IAL
                ( SAL ( Unary BNot ) )
            )
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 0 Copy )
                )
            )
        , Lambda
            [ Inst
                ( IAL ( SAL Discard ) )
            , Inst
                ( IAL
                    ( SAL
                        ( SStatic 0 Copy )
                    )
                )
            , Inst
                ( IAL
                    ( SAL
                        ( Cons 10 )
                    )
                )
            , Inst
                ( IAL
                    ( SAL ( Binary LEQ ) )
                )
            , Inst
                ( IAL
                    ( SAL ( Unary BNot ) )
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
        ( IAL ( SAL Discard ) )
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
    ( IAL ( SAL Discard ) )
, Str "</table>
  "
]