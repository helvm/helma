{ fbreak.f -- FALSE Breakout by Ed Mackey. }

{ Amiga version in original False distribution. }
{ Port from Amiga version by Benjamin Hoyt, needs ANSI.SYS }
{ Must be compiled under my 80386 FalseCom }

{ d = delay -- adjust to suit computer speed, 5000 is ok for my 386 :-) }
5000d:

{ keys during the game:
    SPACE  - release ball
    j      - move paddle left
    l      - move paddle right
    q      - abort game.
}

ß

{ variables }
{ s = Score, p = PaddleX, x = BallX, y = BallY, h = horizontalC, v = verticalC }
{ b = brick array, j = horizDir, k = vertDir, m = hcReset, n = vcReset, r = bricksRemaining }
{ Reserved for functions: w, c, g. }
{ Temp variable local to CheckBrick(): t }

{ BOOL GotOne = WaitForChar(int timeout) }
[102`88`102`43`201`180`1`205`22`116`2`102`73`102`81`]w:

{ int GetChar(void) }
[180`0`205`22`102`15`182`192`102`80`]z:

{ void CheckPaddle(int newx, int newy) }
[
    { if } 22= { then }
    [
        $+ p;$+9+ - {x relative to paddle}
        { if } $$10_> \9>~ & { then }
        [
            1_k:    {ball bounces up}
            1j: {and to the right}
            { if } $0>~ { then }
            [
                1_j:    {er, make that the left}
                _   {and make rel-x positive}
            ]? {end if}
            1$h:v:      {get moving}
            5+$n:15\-m: {new speeds}
            0
        ]? {end if}
        %
    ]? {end if}
]g:

{ BOOL CheckBrick(int newx, int newy)   Returns TRUE for collision. }
[
    { if } $1= 2ø0=| 2ø81=|~ { then }   {if ball is in bounds,}
    $[
        @@
        0t:
        { if } $2> 1ø7>~& { then }
        [
            $3- $+$+ b;+$;  { Read row of bricks & pointer }

            { stack: truth, x, y, pointer, brickmap }

            3ø1-5/@@2ø  { brick x }
            1
            { while }[1ø][$+\1-\]#\% {end while}

            { stack: truth, x, y, brick x, pointer, brickmap, mask }

            { if } $2ø& { then }  {collision here?}
            [
                1t:
                s;1+$s: 27,91,"1;12H".  { Print new score }
                27,91,4ø.";"3ø5*1+."H     " { Erase brick from screen }
                r;1-r:      { Decrease onscreen brick counter }
            ]? {end if}

            ~&\:%   { delete brick's bit if needed }
        ]? {end if}
        %%t;
        \
    ]? { else } ~[
        %%1_
    ]? {end if}
]c:

{ MAIN PROGRAM }

{ clear screen & print header }
27,"[H"27,"[J    Score: 0                  FBreak by Ed Mackey."

1_$$$$  { init five rows of bricks, 1 bit each.  Only low 16 bits used. }
102`43`192`139`196`102`80`b:  { sub eax,eax; mov ax,sp; push eax }
{ gets a pointer (b) to the rows }

0s: { Score starts at 0 }
0r: { No bricks on screen yet }
36p:    { Paddle starts in middle }
1h:1v:  { horiz & vert counters of ball }
1j:1_k: { horiz & vert directions of ball }
10$m:n: { horiz & vert speed of ball (counter's underflow reset values) }

{ Print the blocks on the screen }
0
{ while }[$ 4 >~]
[
    27,91, $3+. ";1H"   { Move cursor to start of row }

    $ $+$+ b;+; { Read row of bricks }

    1
    { while }[$ 65535 >~]
    [
        { if } $ 2ø & {then}0\[~]?
        $[
            "<"27,91,"7m==="27,91,"0m>"
            r;1+r:  { Increase number of onscreen bricks }
        ]? {else} ~[
            "     "
        ]? {end if}

        $+  { rotate left 1 bit }
    ]#% {end while}

    %  {Don't need row of bricks}

    1+
]#% {end while}

{ Render the paddle }
27,91,"22;"p;."H<======>"
27,91,"21;"p;4+."H*"
27,91,"H"ß

{ Initial aiming phase }
{ while }[z;!$$$1_=~@'q=~&@32=~&]    {char is not EOF, q, or space}
[
    { if } $'j= p;1> & { then }
    [
        p;1-p:  {move paddle left}
        27,91,"22;"p;."H<======> "
        27,91,"21;"p;4+."H* "
        27,91,"H"ß
    ]? {end if}

    { if } 'l= 73p;> & { then }
    [
        27,91,"22;"p;."H <======>"
        27,91,"21;"p;4+."H *"
        27,91,"H"ß
        p;1+p:  {move paddle right}
    ]? {end if}
]# {end while}

21y:p;4+x:  { ball's initial X and Y }

{ Main game, ball in flight }
{ while }[0y;22>~r;0>&[%25000w;!$[z;!$$1_=~@'q=~&@]?~[0ß1_]?]?]
[
        { delay loop } d;[$][1-]#%

    { if } $155= {then} [%z;!]? {end if}

    0   { don't send cursor home }

    { if } 1ø'j= p;1> & { then }
    [
        p;1-p:  {move paddle left}
        27,91,"22;"p;."H<======> "
        { if } y;22= x;p;1-= & { then }
        [
            1_k:    {ball bounces up}
            1$h:v:  {get moving}
            2m:15n: {new speeds}
        ]? {end if}
        %1  {send cursor home}
    ]? {end if}

    { if } \'l= 73p;> & { then }
    [
        27,91,"22;"p;."H <======>"
        p;1+p:  {move paddle right}
        { if } y;22= x;p;8+= & { then }
        [
            1_k:    {ball bounces up}
            1$h:v:  {get moving}
            2m:15n: {new speeds}
        ]? {end if}
        %1  {send cursor home}
    ]? {end if}

    { if } 1h;1-$h:> { then }   { if (1 > --h) ball moves horizontally }
    [
        m;h:

        x;j;+ y; g;!    {check if it would hit the paddle}

        { while } [x;j;+ y; c;!]
        [
            j;_j:
        ]# {end while}

        { if } j;0> { then }
        $[
            27,91,y;.";"x;."H *"
            x;1+ x:
        ]? { else } ~[
            x;1- x:
            27,91,y;.";"x;."H* "
        ]? {end if}
        %1  {send cursor home}
    ]? {end if}

    { if } 1v;1-$v:> { then }   { if (1 > --v) ball moves vertically }
    [
        n;v:

        x; y;k;+ g;!    {check if it would hit the paddle}

        { while } [x; y;k;+ c;!]
        [
            k;_k:
        ]# {end while}

        { if } k;0> { then }
        $[
            y;1+ y:
            27,91,y;.";"x;."H*"8,11," "
        ]? { else } ~[
            27,91,y;.";"x;."H "8,11,"*"
            y;1- y:
        ]? {end if}
        %1  {send cursor home}
    ]? {end if}

    { if cursor needs to be sent home, then }
    [
        27,91,"H"
    ]? {end if}
]# {end while}

{ Game Over message }
27,91,"10;30H ******************* "
{ if } r;0>$ { then }
[
    27,91,"11;30H **   GAME OVER   ** "
]? { else } ~[
    27,91,"11;30H **   YOU WIN!!   ** "
]? {end if}
27,91,"12;30H **   Press 'q'   ** "
27,91,"13;30H ******************* "
27,91,"H"ß[z;!$1_=~\'q=~&][]#ß27,"[H"27,"[J"ß

{ END of fbreak.f }
