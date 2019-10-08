&Scoped-define TABLENAME gl-jrn

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{custom/globdefs.i}

DEF BUFFER bf-jrn FOR gl-jrn.
DEF VAR li-next-jno AS INT NO-UNDO.

/* gdm - 11050906 */
REPEAT:
 FIND FIRST gl-ctrl EXCLUSIVE-LOCK
   WHERE gl-ctrl.company EQ g_company NO-ERROR NO-WAIT.
 IF AVAIL gl-ctrl THEN DO:

   FIND LAST bf-jrn USE-INDEX j-no NO-LOCK NO-ERROR.
   li-next-jno = (IF AVAIL bf-jrn THEN bf-jrn.j-no ELSE 0) + 1.

   ASSIGN gl-jrn.j-no     = li-next-jno 
          gl-jrn.journal  = gl-ctrl.journal + 1
          gl-ctrl.journal = gl-jrn.journal
          gl-jrn.tr-date  = TODAY.
   FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
   LEAVE.
 END. /* IF AVAIL gl-ctrl */
END. /* REPEAT */
/* gdm - 11050906 */
