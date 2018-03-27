&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME e-item-vend

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

DEF VAR v-count AS INT NO-UNDO.

{methods/triggers/write.i}

{custom/e-item-edit.i {&TABLENAME}}
    
DO v-count = 1 TO 10:

   IF old-{&TABLENAME}.run-qty[v-count] NE {&TABLENAME}.run-qty[v-count] OR
      old-{&TABLENAME}.run-cost[v-count] NE {&TABLENAME}.run-cost[v-count] OR
      old-{&TABLENAME}.setups[v-count] NE {&TABLENAME}.setups[v-count] THEN
      DO:
         ASSIGN
            e-item-vend.updated-id[1] = USERID("NOSWEAT")
            e-item-vend.updated-date[1] = TODAY.
         LEAVE.
      END.
END.
