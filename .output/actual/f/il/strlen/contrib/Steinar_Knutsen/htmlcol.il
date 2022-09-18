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
        , Inst ( IAL Dup )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Unary Neg ) )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Inst
            ( IAL ( Unary BNot ) )
        , Inst ( IAL Dup )
        , Lambda
            [ Inst ( IAL Discard )
            , Inst ( IAL Dup )
            , Inst
                ( IAL
                    ( Cons 10 )
                )
            , Inst
                ( IAL ( Binary LEQ ) )
            , Inst
                ( IAL ( Unary BNot ) )
            ]
        , Cond
        ]
    , Lambda
        [ Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , While
    , Inst ( IAL Discard )
    , Str "</td>"
    ]
, Ref 15
, Store
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Unary Neg ) )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
, Lambda
    [ Str "<tr>
      <td>"
    , Inst
        ( IAL ( SIO OutputChar ) )
    , Lambda
        [ Inst
            ( IAL ( SIO InputChar ) )
        , Inst ( IAL Dup )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Unary Neg ) )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Inst
            ( IAL ( Unary BNot ) )
        , Inst ( IAL Dup )
        , Lambda
            [ Inst ( IAL Discard )
            , Inst ( IAL Dup )
            , Inst
                ( IAL
                    ( Cons 10 )
                )
            , Inst
                ( IAL ( Binary LEQ ) )
            , Inst
                ( IAL ( Unary BNot ) )
            ]
        , Cond
        ]
    , Lambda
        [ Inst
            ( IAL ( SIO OutputChar ) )
        ]
    , While
    , Inst ( IAL Discard )
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
, Inst ( IAL Discard )
, Str "</table>
  "
]