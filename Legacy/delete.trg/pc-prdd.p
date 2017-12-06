&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME pc-prdd

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

/* not delete if estimate exists */
FIND est WHERE est.rec_key = pc-prdd.rec_key NO-LOCK NO-ERROR.
IF NOT AVAIL est THEN DO:
    {methods/triggers/delete.i}
END.

DEF BUFFER b-pc-prdd FOR pc-prdd.

FIND FIRST pc-prdh EXCLUSIVE-LOCK
    WHERE pc-prdh.company    EQ pc-prdd.company
      AND pc-prdh.m-code     EQ pc-prdd.m-code
      AND pc-prdh.trans-date EQ pc-prdd.op-date
      AND pc-prdh.shift      EQ pc-prdd.shift
    NO-ERROR.
IF AVAIL pc-prdh AND
   NOT can-find(FIRST b-pc-prdd
      WHERE b-pc-prdd.company EQ pc-prdh.company
        AND b-pc-prdd.m-code  EQ pc-prdh.m-code
        AND b-pc-prdd.op-date EQ pc-prdh.trans-date
        AND b-pc-prdd.shift   EQ pc-prdh.shift
        AND ROWID(b-pc-prdd)  NE ROWID(pc-prdd)) THEN
  
  DELETE pc-prdh.

