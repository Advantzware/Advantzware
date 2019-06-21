&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME e-itemfg-vend

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

IF {&TABLENAME}.company NE ""     AND
   {&TABLENAME}.roll-w[27] EQ 0   AND
   {&TABLENAME}.roll-w[28] EQ 0   AND
   {&TABLENAME}.roll-w[29] EQ 0   AND
   {&TABLENAME}.roll-w[30] EQ 0   THEN DO:
  FIND FIRST b-{&TABLENAME} NO-LOCK
      WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
        AND b-{&TABLENAME}.i-no     EQ {&TABLENAME}.i-no
        AND b-{&TABLENAME}.vend-no  EQ ""
        AND b-{&TABLENAME}.est-no   EQ {&TABLENAME}.est-no
        AND b-{&TABLENAME}.eqty     EQ {&TABLENAME}.eqty
        AND b-{&TABLENAME}.form-no  EQ {&TABLENAME}.form-no
        AND b-{&TABLENAME}.blank-no EQ {&TABLENAME}.blank-no
        AND ROWID(b-{&TABLENAME})   NE ROWID({&TABLENAME})
      NO-ERROR.
  IF AVAIL b-{&TABLENAME} THEN
    ASSIGN
     {&TABLENAME}.roll-w[27] = b-{&TABLENAME}.roll-w[27]
     {&TABLENAME}.roll-w[28] = b-{&TABLENAME}.roll-w[28]
     {&TABLENAME}.roll-w[29] = b-{&TABLENAME}.roll-w[29]
     {&TABLENAME}.roll-w[30] = b-{&TABLENAME}.roll-w[30].
  ELSE
    ASSIGN
     {&TABLENAME}.roll-w[28] = 999.9999
     {&TABLENAME}.roll-w[30] = 999.9999.
END.

{custom/e-itemfg-edit.i {&TABLENAME}}
