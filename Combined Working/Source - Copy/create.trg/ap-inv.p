&Scoped-define TABLENAME ap-inv

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

DEF BUFFER bf-inv FOR ap-inv.


FIND LAST bf-inv USE-INDEX i-no NO-LOCK NO-ERROR.
ASSIGN
 {&TABLENAME}.i-no   = (IF AVAIL bf-inv THEN bf-inv.i-no ELSE 0) + 1
 {&TABLENAME}.inv-no = FILL(" ",100) + STRING({&TABLENAME}.i-no,"9999999999").
