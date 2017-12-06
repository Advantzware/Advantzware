&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME routing-mtx

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR lk AS INT NO-UNDO.


IF old-{&TABLENAME}.company NE "" THEN
DO li = 1 TO EXTENT({&TABLENAME}.r-code):
  lj = li MODULO 10.
  IF lj EQ 0 THEN lj = 10.

  lk = (li - lj + 10) / 10.

  IF lk GT 0 AND lk LE EXTENT({&TABLENAME}.bl-wid) AND
     lj GT 0 AND lj LE EXTENT({&TABLENAME}.bl-len) THEN
    IF {&TABLENAME}.bl-wid[lk] LE 0 OR
       {&TABLENAME}.bl-len[lj] LE 0 THEN {&TABLENAME}.r-code[li] = "".
END.
