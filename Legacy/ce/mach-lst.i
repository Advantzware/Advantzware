
def {1} shared workfile m-lst
    field seq           as   int
    field bl            as   log
    field f-no          as   int
    field b-no          as   int
    field n-out         like est-op.n-out
    field dept          as   char
    field m-code        like mach.m-code
    field dscr          like mach.m-dscr
    field pass-no       as   int
    field dim-pos       as   char  /* Based on LWD w/W siqnifying the dimension
                                      being cut down by the machine.  */
    field dim           as   dec extent 3
    field defr          as   log init no
    field defr-valid    as   log init no
    field styl          as   log init no.
                                      
def buffer mm-lst for m-lst.
