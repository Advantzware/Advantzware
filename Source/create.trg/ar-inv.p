&Scoped-define TABLENAME ar-inv

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

DEFINE VARIABLE iNextXNo AS INTEGER NO-UNDO.

iNextXNo = NEXT-VALUE (arInvXno_Seq).

DO WHILE CAN-FIND(LAST ar-inv WHERE ar-inv.x-no EQ iNextXNo):
    iNextXNo = NEXT-VALUE (arInvXno_Seq).            
END.  

{&TABLENAME}.x-no = iNextXNo.
