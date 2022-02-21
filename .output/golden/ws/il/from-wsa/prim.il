[ IControl
    ( Mark "st" )
, IAL
    ( Cons 0 )
, IAL
    ( Cons 93 )
, IAL
    ( Cons 48 )
, IAL
    ( Cons 48 )
, IAL
    ( Cons 49 )
, IAL
    ( Cons 45 )
, IAL
    ( Cons 50 )
, IAL
    ( Cons 91 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 115 )
, IAL
    ( Cons 114 )
, IAL
    ( Cons 101 )
, IAL
    ( Cons 98 )
, IAL
    ( Cons 109 )
, IAL
    ( Cons 117 )
, IAL
    ( Cons 110 )
, IAL
    ( Cons 32 )
, IAL
    ( Cons 109 )
, IAL
    ( Cons 105 )
, IAL
    ( Cons 114 )
, IAL
    ( Cons 112 )
, IControl
    ( CStatic "printsln" Call )
, IAL
    ( Cons 0 )
, IControl
    ( Mark "f_start_1" )
, IAL Dup
, IAL
    ( Cons 100 )
, IAL ( Binary Sub )
, IControl
    ( CStatic "f_finished_1" ( Branch EZ ) )
, IAL Dup
, IAL
    ( Cons 100 )
, IAL ( Binary Add )
, IAL
    ( Cons 1 )
, ILS Store
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IControl
    ( CStatic "f_start_1" Jump )
, IControl
    ( Mark "f_finished_1" )
, IAL Discard
, IAL
    ( Cons 2 )
, IControl
    ( Mark "f_start_2" )
, IAL Dup
, IAL
    ( Cons 11 )
, IAL ( Binary Sub )
, IControl
    ( CStatic "f_finished_2" ( Branch EZ ) )
, IAL
    ( Cons 2 )
, IControl
    ( Mark "f_start_3" )
, IAL Dup
, IAL
    ( Cons 100 )
, IAL ( Binary Sub )
, IControl
    ( CStatic "f_finished_3" ( Branch EZ ) )
, IAL Dup
, IAL
    ( Cons 1 )
, IAL Swap
, ILS Store
, IAL Swap
, IAL Dup
, IAL
    ( Cons 2 )
, IAL Swap
, ILS Store
, IAL Swap
, IAL
    ( Cons 1 )
, ILS Load
, IAL
    ( Cons 2 )
, ILS Load
, IAL ( Binary Mul )
, IAL
    ( Cons 100 )
, IAL ( Binary Add )
, IAL
    ( Cons 0 )
, ILS Store
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IControl
    ( CStatic "f_start_3" Jump )
, IControl
    ( Mark "f_finished_3" )
, IAL Discard
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IControl
    ( CStatic "f_start_2" Jump )
, IControl
    ( Mark "f_finished_2" )
, IAL Discard
, IAL
    ( Cons 2 )
, IControl
    ( Mark "f_start_4" )
, IAL Dup
, IAL
    ( Cons 100 )
, IAL ( Binary Sub )
, IControl
    ( CStatic "f_finished_4" ( Branch EZ ) )
, IAL Dup
, IAL
    ( Cons 100 )
, IAL ( Binary Add )
, ILS Load
, IControl
    ( CStatic "nodraw" ( Branch EZ ) )
, IAL Dup
, IAL ( SIO OutputDec )
, IAL
    ( Cons 0 )
, IAL
    ( Cons 32 )
, IControl
    ( CStatic "prints" Call )
, IControl
    ( Mark "nodraw" )
, IAL
    ( Cons 1 )
, IAL ( Binary Add )
, IControl
    ( CStatic "f_start_4" Jump )
, IControl
    ( Mark "f_finished_4" )
, IAL Discard
, IAL
    ( Cons 0 )
, IControl
    ( CStatic "printsln" Call )
, End
, IControl
    ( Mark "prints" )
, IAL Dup
, IControl
    ( CStatic "prints_end" ( Branch EZ ) )
, IAL ( SIO OutputChar )
, IControl
    ( CStatic "prints" Jump )
, IControl
    ( Mark "prints_end" )
, IAL Discard
, IControl Return
, IControl
    ( Mark "printsln" )
, IControl
    ( CStatic "prints" Call )
, IAL
    ( Cons 10 )
, IAL ( SIO OutputChar )
, IControl Return
]