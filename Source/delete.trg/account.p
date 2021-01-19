&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME account

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

IF CAN-FIND(FIRST glhist
            WHERE glhist.company EQ {&TABLENAME}.company
              AND glhist.actnum  EQ {&TABLENAME}.actnum)  OR
   CAN-FIND(FIRST gltrans
            WHERE gltrans.company EQ {&TABLENAME}.company
              AND gltrans.actnum  EQ {&TABLENAME}.actnum) OR 
   CAN-FIND(FIRST gl-jrnl 
            WHERE gl-jrnl.actnum EQ {&TABLENAME}.actnum)THEN DO:
  RUN displayMessage ("18").
  RETURN ERROR.
END.

{methods/triggers/delete.i}
