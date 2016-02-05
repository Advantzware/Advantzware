&Scoped-define TABLENAME quotehd

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{custom/globdefs.i}

DEF VAR li AS INT NO-UNDO.

REPEAT:
        
   FIND FIRST ce-ctrl where
        ce-ctrl.company = g_company and 
        ce-ctrl.loc     = g_loc
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   IF AVAIL ce-ctrl THEN
   DO:
      ASSIGN
         ce-ctrl.q-num = ce-ctrl.q-num + 1.
         li = ce-ctrl.q-num.

      FIND FIRST ce-ctrl WHERE
           ce-ctrl.company = g_company and 
           ce-ctrl.loc     = g_loc
           NO-LOCK.
      LEAVE.
   END.
END.

ASSIGN
 {&TABLENAME}.company = g_company
 {&TABLENAME}.loc     = g_loc
 {&TABLENAME}.q-no    = li.

