{ tic tac toe }

{ written in false }
{ by Marcel van Kervinck }
{ e-mail <marcelk@stack.urc.tue.nl> }
{ June 1994 }

{ for all }
[[$0=~][$$_&$_1-@&\2O!]#%%]f:

{ print board }
[["   "$x;\/\o;\/[32,$4O&0=~$["O"\%]?~[4O&0=~$["X"]?~["."]?
]?]7f;!10,%%]"7 8 9"$64\!"4 5 6"$8\!"1 2 3"1\!]b:

{ test three-in-a-row }
[0$7 56 448 73 146 292 84 273 10O[\$0>][$2O&=[%[0>][]#1-0$]?]#%%\%]t:

{ compute evaluation }
[1Ot;!$~[%$@$@@|_1-511&2_[5O|4Oe;!_a:@a;$@$@@>~[\]?%$1=[%%0
1]?@@]@f;!$2_=[%0]?]?\%\%]e:

"tic tac toe
"
xp:oq:'y['y=][0x:0o:10,b;![o;x;|_1-511&$0>q;;t;!~&]
[xp;=$[\0[$0=][%"your move: "B^B'1-1\[$0>][\$+\1-]#%1O&]#\%\]?~
[$511=[%19]?"my move: "B0w:2v:[$u:p;;|q;;e;!$v;\>[$v:u;w:]?%]\f;!
0w;[$0>][\1+\2/]#%.10,w;]? p;;|p;:10,b;!p;q;p:q:]#%
x;t;!$["you win"]?~[o;t;!$["I win"]?~["draw"]?]?
" -- another game? (y/n) "B^B]#

