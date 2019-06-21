&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ap-invl

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.
DEFINE SHARED VAR g_user AS CHAR NO-UNDO.

{methods/triggers/write.i}

{sys/inc/var.i NEW SHARED}

{custom/globdefs.i}

ASSIGN
 cocode = g_company
 locode = g_loc.

FOR EACH ap-inv WHERE ap-inv.i-no EQ {&TABLENAME}.i-no EXCLUSIVE:
  ap-inv.user-id = g_user .
END.
