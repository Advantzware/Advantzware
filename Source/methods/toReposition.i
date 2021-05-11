/* toReposition.i */

&IF DEFINED(h_Object{1}) NE 0 &THEN
    FIND FIRST toreposition
         WHERE toreposition.widhand EQ STRING({&h_Object{1}})
         NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
    CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object{1}})
        toreposition.resizepage = pgno
        toreposition.widtype    = "movedown"
        .
    &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP("{&h_Object{1}}","{&moveRight}") GT 0 THEN
    toreposition.widtype = "moveright".
    &ENDIF
&ENDIF
