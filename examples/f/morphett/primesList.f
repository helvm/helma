{ Lists all prime numbers <= 10. }

[\$@$@\/*-0=]d:  {Test if p divides q}

[$2/[\$@$@\d;!~][1-]#1=\%]p:   {Is p prime?}

[[$1=~][$p;![$." "]?1-]#]f:  {for all i from n to 2 do { if i is prime then print i} }

10f;!

{Or, without comments and functions:

[[$1=~][$$2/[\$@$@\\$@$@\/*-0=~][1-]#1=\%[$." "]?1-]#]f:

}
