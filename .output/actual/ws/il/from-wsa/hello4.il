[ IAL
    ( Cons 0 )
, IAL
    ( Cons 33 )
, IAL
    ( Cons 100 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 114 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 119 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 44 )
, IAL
    ( Cons 111 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 108 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 72 )
, ICF
    ( CStatic "printsln" Call )
, End
, ICF
    ( SMark "prints" )
, IAL
    ( SStatic 0 Copy )
, ICF
    ( CStatic "prints_end" ( Branch EZ ) )
, IAL ( SIO OutputChar )
, ICF
    ( CStatic "prints" Jump )
, ICF
    ( SMark "prints_end" )
, IAL Discard
, ICF Return
, ICF
    ( SMark "printsln" )
, ICF
    ( CStatic "prints" Call )
, IAL
    ( Cons 10 )
, IAL ( SIO OutputChar )
, ICF Return
]