{ Futz.f: Fix a FALSE file for fantastically fun formatting. }
{ by Ed Mackey }

0c:0d:

{ mode: 0 = start, outside comment
	1 = start, inside comment
	2 = normal program
	3 = inside quotes
	4 = inside comment
}

{ void PrintChar(char ch) }
[
	$$47> \57>~&d:
	$9=
	[
		c;8/8*7+c:
	]?
	c;1+c:
	$,10=
	[
		0c:
	]?
]p:

�0[^$1_=~]
[
	\$
	{ Stack: char, newmode, curmode }
	$0=[
		2�34=	{quote}
		[
			\%3\
		]?
		2�123=	{comment start}
		[
			\%1\
		]?
		2�$$$$ 32= \9=| \10=| \34=| \123=| ~
		[
			\%2\
		]?
		2�p;!
	]?
	$1=[
		2�125=	{comment end}
		[
			\%0\
		]?
		2�p;!
	]?
	$2=[
		2�34=	{quote}
		[
			\%3\
			c;73>[10p;!]?
			34p;!
		]?
		2�123=	{comment start}
		[
			\%4\
		]?
		2�$$$ 32= \9=| \10=| \123=| d;&
		[
			2d:
		]?
		2�$$$$ 32= \9=| \10=| \34=| \123=| ~
		[
			2�$$47>~ \57>| $[c;75>[10p;!]?]?
			~d;0=&[c;72>[10p;!]?]?
			$$47> \57>~& 2d;=&
			[
				c;74>$[10p;!]?~[32p;!]?
			]?
			p;!
		]?
	]?
	$3=[
		2�34=	{quotes end}
		[
			\%2\
			34p;!
		]?
		2�34=~
		[
			c;74>[34p;!10p;!34p;!]?
			2�p;!
		]?
	]?
	$4=[
		2�125=	{comment end}
		[
			\%2\
		]?
	]?
	{ Stack: char, newmode, curmode }
	%\%
	{Stack: newmode}
]#10,
