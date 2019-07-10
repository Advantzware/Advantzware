&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ef-nsh

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

/* DEFINE VARIABLE v-len-in  LIKE {&TABLENAME}.len-in  NO-UNDO INIT 0.  */
/* DEFINE VARIABLE v-wid-in  LIKE {&TABLENAME}.wid-in  NO-UNDO INIT 0.  */
/* DEFINE VARIABLE v-dep-in  LIKE {&TABLENAME}.dep-in  NO-UNDO INIT 0.  */
/* DEFINE VARIABLE v-n-out-l LIKE {&TABLENAME}.n-out-l NO-UNDO INIT 0.  */
/* DEFINE VARIABLE v-n-out-w LIKE {&TABLENAME}.n-out-w NO-UNDO INIT 0.  */
/* DEFINE VARIABLE v-n-out-d LIKE {&TABLENAME}.n-out-d NO-UNDO INIT 0.  */
/* DEFINE VARIABLE v-len-out LIKE {&TABLENAME}.len-out NO-UNDO INIT 0.  */
/* DEFINE VARIABLE v-wid-out LIKE {&TABLENAME}.wid-out NO-UNDO INIT 0.  */
/* DEFINE VARIABLE v-dep-out LIKE {&TABLENAME}.dep-out NO-UNDO INIT 0.  */

DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF ef.
DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.

{&TABLENAME}.est-no = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                      TRIM({&TABLENAME}.est-no).

FIND FIRST ef OF {&TABLENAME} NO-LOCK NO-ERROR.

IF AVAIL ef THEN DO:

/*     /* ------ stacey's change ------ */                           */
/*     MESSAGE "Recalculate totals"                                  */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */
/*                                                                   */
/*     /* Multiply WLD from all ef-nsh records. */                   */
/*     FOR EACH b-{&TABLENAME} OF ef NO-LOCK:                        */
/*         IF b-{&TABLENAME}.pass-no = 1 THEN                        */
/*             ASSIGN v-len-in = b-{&TABLENAME}.len-in               */
/*                    v-wid-in = b-{&TABLENAME}.wid-in               */
/*                    v-dep-in = b-{&TABLENAME}.dep-in               */
/*                    v-n-out-l = b-{&TABLENAME}.n-out-l             */
/*                    v-n-out-w = b-{&TABLENAME}.n-out-w             */
/*                    v-n-out-d = b-{&TABLENAME}.n-out-d             */
/*                    v-len-out = b-{&TABLENAME}.len-out             */
/*                    v-wid-out = b-{&TABLENAME}.wid-out             */
/*                    v-dep-out = b-{&TABLENAME}.dep-out.            */
/*         ELSE                                                      */
/*         ASSIGN v-len-in = (v-len-in * b-{&TABLENAME}.len-in)      */
/*                v-wid-in = (v-wid-in * b-{&TABLENAME}.wid-in)      */
/*                v-dep-in = (v-dep-in * b-{&TABLENAME}.dep-in)      */
/*                v-n-out-l = (v-n-out-l * b-{&TABLENAME}.n-out-l)   */
/*                v-n-out-w = (v-n-out-w * b-{&TABLENAME}.n-out-w)   */
/*                v-n-out-d = (v-n-out-d * b-{&TABLENAME}.n-out-d)   */
/*                v-len-out = (v-len-out * b-{&TABLENAME}.len-out)   */
/*                v-wid-out = (v-wid-out * b-{&TABLENAME}.wid-out)   */
/*                v-dep-out = (v-dep-out * b-{&TABLENAME}.dep-out).  */
/*     END.                                                          */
/*                                                                   */
/*     MESSAGE "ef totals before update"                             */
/*            "ef.gsh-len" ef.gsh-len  SKIP                          */
/*            "ef.gsh-wid" ef.gsh-wid   SKIP                         */
/*            "ef.gsh-dep" ef.gsh-dep SKIP                           */
/*            "ef.n-out-l" ef.n-out-l SKIP                           */
/*            "ef.n-out" ef.n-out   SKIP                             */
/*            "ef.n-out-d" ef.n-out-d SKIP                           */
/*            "ef.nsh-len" ef.nsh-len SKIP                           */
/*            "ef.nsh-wid" ef.nsh-wid SKIP                           */
/*            "ef.nsh-dep" ef.nsh-dep                                */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */
/*                                                                   */
/*     /* Update final totals to the ef record. */                   */
/*     FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR NO-WAIT.              */
/*     ASSIGN ef.gsh-len = v-len-in                                  */
/*            ef.gsh-wid = v-wid-in                                  */
/*            ef.gsh-dep = v-dep-in                                  */
/*            ef.n-out-l = v-n-out-l                                 */
/*            ef.n-out   = v-n-out-w                                 */
/*            ef.n-out-d = v-n-out-d                                 */
/*            ef.nsh-len = v-len-out                                 */
/*            ef.nsh-wid = v-wid-out                                 */
/*            ef.nsh-dep = v-dep-out.                                */
/*                                                                   */
/*     /* Release lock. */                                           */
/*     FIND CURRENT ef NO-LOCK.                                      */
/*                                                                   */
/*     MESSAGE "ef totals after update"                              */
/*            "ef.gsh-len" ef.gsh-len  SKIP                          */
/*            "ef.gsh-wid" ef.gsh-wid   SKIP                         */
/*            "ef.gsh-dep" ef.gsh-dep SKIP                           */
/*            "ef.n-out-l" ef.n-out-l SKIP                           */
/*            "ef.n-out" ef.n-out   SKIP                             */
/*            "ef.n-out-d" ef.n-out-d SKIP                           */
/*            "ef.nsh-len" ef.nsh-len SKIP                           */
/*            "ef.nsh-wid" ef.nsh-wid SKIP                           */
/*            "ef.nsh-dep" ef.nsh-dep                                */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */


/*   IF {&TABLENAME}.pass-no EQ 1 THEN DO:     */
/*     FIND CURRENT ef.                        */
/*                                             */
/*     MESSAGE "ef.n-out: " ef.n-out SKIP      */
/*             "ef.n-out-l: " ef.n-out-l SKIP  */
/*             "ef.n-out-d: " ef.n-out-d       */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
/*     ASSIGN                                  */
/*      ef.gsh-len = {&TABLENAME}.len-in       */
/*      ef.gsh-wid = {&TABLENAME}.wid-in       */
/*      ef.gsh-dep = {&TABLENAME}.dep-in       */
/*      ef.n-out-l = {&TABLENAME}.n-out-l      */
/*      ef.n-out   = {&TABLENAME}.n-out-w      */
/*      ef.n-out-d = {&TABLENAME}.n-out-d      */
/*      ef.nsh-len = {&TABLENAME}.len-out      */
/*      ef.nsh-wid = {&TABLENAME}.wid-out      */
/*      ef.nsh-dep = {&TABLENAME}.dep-out.     */
/*     FIND CURRENT ef NO-LOCK.                */
/*                                             */
/*                                             */
/*     MESSAGE "ef.n-out: " ef.n-out SKIP      */
/*             "ef.n-out-l: " ef.n-out-l SKIP  */
/*             "ef.n-out-d: " ef.n-out-d       */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
/*                                             */
/*   END. /* IF {&TABLENAME}.pass-no EQ 1 */   */

  IF {&TABLENAME}.pass-no NE old-{&TABLENAME}.pass-no AND
     old-{&TABLENAME}.pass-no NE 0                    THEN
  FOR EACH b-{&TABLENAME} OF ef
      WHERE b-{&TABLENAME}.orig-no EQ old-{&TABLENAME}.pass-no
        AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME}):
     ASSIGN b-{&TABLENAME}.orig-no = {&TABLENAME}.pass-no.
  END. /* FOR EACH b-{&TABLENAME} OF ef */
END. /* IF AVAIL ef  */

FIND FIRST est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no
    NO-ERROR.

IF AVAIL est THEN DO:
  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat")
   est.mod-date     = est.updated-date.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
