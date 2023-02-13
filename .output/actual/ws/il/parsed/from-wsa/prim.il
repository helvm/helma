[ ICF
    ( SMark "st" )
, IAL
    ( SAL
        ( Cons 0 )
    )
, IAL
    ( SAL
        ( Cons 93 )
    )
, IAL
    ( SAL
        ( Cons 48 )
    )
, IAL
    ( SAL
        ( Cons 48 )
    )
, IAL
    ( SAL
        ( Cons 49 )
    )
, IAL
    ( SAL
        ( Cons 45 )
    )
, IAL
    ( SAL
        ( Cons 50 )
    )
, IAL
    ( SAL
        ( Cons 91 )
    )
, IAL
    ( SAL
        ( Cons 32 )
    )
, IAL
    ( SAL
        ( Cons 115 )
    )
, IAL
    ( SAL
        ( Cons 114 )
    )
, IAL
    ( SAL
        ( Cons 101 )
    )
, IAL
    ( SAL
        ( Cons 98 )
    )
, IAL
    ( SAL
        ( Cons 109 )
    )
, IAL
    ( SAL
        ( Cons 117 )
    )
, IAL
    ( SAL
        ( Cons 110 )
    )
, IAL
    ( SAL
        ( Cons 32 )
    )
, IAL
    ( SAL
        ( Cons 109 )
    )
, IAL
    ( SAL
        ( Cons 105 )
    )
, IAL
    ( SAL
        ( Cons 114 )
    )
, IAL
    ( SAL
        ( Cons 112 )
    )
, ICF
    ( CStatic "printsln" Call )
, IAL
    ( SAL
        ( Cons 0 )
    )
, ICF
    ( SMark "f_start_1" )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( Cons 100 )
    )
, IAL
    ( SAL ( Binary Sub ) )
, ICF
    ( CStatic "f_finished_1" ( Branch EZ ) )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( Cons 100 )
    )
, IAL
    ( SAL ( Binary Add ) )
, IAL
    ( SAL
        ( Cons 1 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 1 )
    )
, IAL
    ( SAL ( Binary Add ) )
, ICF
    ( CStatic "f_start_1" Jump )
, ICF
    ( SMark "f_finished_1" )
, IAL ( SAL Discard )
, IAL
    ( SAL
        ( Cons 2 )
    )
, ICF
    ( SMark "f_start_2" )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( Cons 11 )
    )
, IAL
    ( SAL ( Binary Sub ) )
, ICF
    ( CStatic "f_finished_2" ( Branch EZ ) )
, IAL
    ( SAL
        ( Cons 2 )
    )
, ICF
    ( SMark "f_start_3" )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( Cons 100 )
    )
, IAL
    ( SAL ( Binary Sub ) )
, ICF
    ( CStatic "f_finished_3" ( Branch EZ ) )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( Cons 1 )
    )
, IAL
    ( SAL
        ( SStatic 1 Move )
    )
, ILS Store
, IAL
    ( SAL
        ( SStatic 1 Move )
    )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( Cons 2 )
    )
, IAL
    ( SAL
        ( SStatic 1 Move )
    )
, ILS Store
, IAL
    ( SAL
        ( SStatic 1 Move )
    )
, IAL
    ( SAL
        ( Cons 1 )
    )
, ILS Load
, IAL
    ( SAL
        ( Cons 2 )
    )
, ILS Load
, IAL
    ( SAL ( Binary Mul ) )
, IAL
    ( SAL
        ( Cons 100 )
    )
, IAL
    ( SAL ( Binary Add ) )
, IAL
    ( SAL
        ( Cons 0 )
    )
, ILS Store
, IAL
    ( SAL
        ( Cons 1 )
    )
, IAL
    ( SAL ( Binary Add ) )
, ICF
    ( CStatic "f_start_3" Jump )
, ICF
    ( SMark "f_finished_3" )
, IAL ( SAL Discard )
, IAL
    ( SAL
        ( Cons 1 )
    )
, IAL
    ( SAL ( Binary Add ) )
, ICF
    ( CStatic "f_start_2" Jump )
, ICF
    ( SMark "f_finished_2" )
, IAL ( SAL Discard )
, IAL
    ( SAL
        ( Cons 2 )
    )
, ICF
    ( SMark "f_start_4" )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( Cons 100 )
    )
, IAL
    ( SAL ( Binary Sub ) )
, ICF
    ( CStatic "f_finished_4" ( Branch EZ ) )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL
    ( SAL
        ( Cons 100 )
    )
, IAL
    ( SAL ( Binary Add ) )
, ILS Load
, ICF
    ( CStatic "nodraw" ( Branch EZ ) )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, IAL ( SIO OutputDec )
, IAL
    ( SAL
        ( Cons 0 )
    )
, IAL
    ( SAL
        ( Cons 32 )
    )
, ICF
    ( CStatic "prints" Call )
, ICF
    ( SMark "nodraw" )
, IAL
    ( SAL
        ( Cons 1 )
    )
, IAL
    ( SAL ( Binary Add ) )
, ICF
    ( CStatic "f_start_4" Jump )
, ICF
    ( SMark "f_finished_4" )
, IAL ( SAL Discard )
, IAL
    ( SAL
        ( Cons 0 )
    )
, ICF
    ( CStatic "printsln" Call )
, End
, ICF
    ( SMark "prints" )
, IAL
    ( SAL
        ( SStatic 0 Copy )
    )
, ICF
    ( CStatic "prints_end" ( Branch EZ ) )
, IAL ( SIO OutputChar )
, ICF
    ( CStatic "prints" Jump )
, ICF
    ( SMark "prints_end" )
, IAL ( SAL Discard )
, ICF Return
, ICF
    ( SMark "printsln" )
, ICF
    ( CStatic "prints" Call )
, IAL
    ( SAL
        ( Cons 10 )
    )
, IAL ( SIO OutputChar )
, ICF Return
]