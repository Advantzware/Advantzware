&Scoped-define TABLENAME quote-vendor

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{custom/globdefs.i}

DEF VAR li AS INT NO-UNDO.

loop1:
REPEAT:
        
   FIND FIRST ce-ctrl where
        ce-ctrl.company = g_company and 
        ce-ctrl.loc     = g_loc
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   IF AVAIL ce-ctrl THEN
   DO:
      ASSIGN
         ce-ctrl.num-len = ce-ctrl.num-len + 1
         li = ce-ctrl.num-len.

      FIND FIRST ce-ctrl WHERE
           ce-ctrl.company = g_company and 
           ce-ctrl.loc     = g_loc
           NO-LOCK.
      LEAVE loop1.
   END.
END.

ASSIGN
 {&TABLENAME}.company = g_company
 {&TABLENAME}.loc     = g_loc
 {&TABLENAME}.q-no    = li.

