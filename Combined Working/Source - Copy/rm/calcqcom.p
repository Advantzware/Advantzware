
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-q-com LIKE item.q-comm NO-UNDO.

def var v           as   int                                            no-undo.
def var v-j-no      like job-mat.j-no                                   no-undo.
def var v-comm      as   dec                                            no-undo.
def var v-bwt       like item.basis-w                                   no-undo.
def var v-len       like item.s-len                                     no-undo.
def var v-wid       like item.s-wid                                     no-undo.
def var v-hld-qty   as   dec                                            no-undo.
DEF VAR lRunFromTrigger AS LOG                                          NO-UNDO.

/* Determine if current program is running from job-mat.p trigger */
RUN get-run-from (INPUT "job-mat", OUTPUT lRunFromTrigger).

/* If this is running from a trigger, don't allow the trigger to execute again */
/* to avoid a cycle in procedure calls                                         */
IF lRunFromTrigger THEN
    DISABLE TRIGGERS FOR LOAD OF job-mat.

FIND item WHERE ROWID(item) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL item AND item.i-code EQ "R" THEN
FOR EACH job
    WHERE job.company EQ item.company
      AND job.opened  EQ YES
    NO-LOCK,
    EACH job-mat
    WHERE job-mat.company EQ job.company
      AND job-mat.job     EQ job.job
      AND job-mat.job-no  EQ job.job-no
      AND job-mat.job-no2 EQ job.job-no2
      AND job-mat.rm-i-no EQ item.i-no
      AND job-mat.all-flg EQ YES
    EXCLUSIVE-LOCK:

  job-mat.qty-iss = 0.

  for each mat-act
      where mat-act.company eq item.company
        and mat-act.job     eq job-mat.job
        and mat-act.job-no  eq job-mat.job-no
        and mat-act.job-no2 eq job-mat.job-no2
        and mat-act.s-num   eq job-mat.frm
        and (mat-act.b-num  eq job-mat.blank-no or job-mat.blank-no eq 0)
        and mat-act.i-no    eq job-mat.i-no
      use-index job no-lock:

    v-hld-qty = mat-act.qty.

    IF job-mat.qty-uom EQ "EA" THEN DO:
      {sys/inc/roundup.i v-hld-qty}
    END.

    job-mat.qty-iss = job-mat.qty-iss + v-hld-qty.
  end.

  IF job-mat.qty-all LT 0 THEN job-mat.qty-all = 0.

  IF job-mat.qty-all EQ 0 THEN job-mat.all-flg = NO.

  if not job-mat.all-flg then next.

  v-comm = job-mat.qty-all.

  if job-mat.qty-uom ne item.cons-uom then do:
    assign
     v-bwt = job-mat.basis-w
     v-len = job-mat.len
     v-wid = job-mat.wid.

    if v-len eq 0 then v-len = item.s-len.

    if v-wid eq 0 then
      v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.

    if v-bwt eq 0 then v-bwt = item.basis-w.

    run sys/ref/convquom.p(job-mat.qty-uom, item.cons-uom,
                           v-bwt, v-len, v-wid, item.s-dep,
                           v-comm, output v-comm).
  end.

  IF v-comm GT 0 THEN op-q-com = op-q-com + v-comm.
end.


PROCEDURE get-run-from:
DEF INPUT PARAMETER ipcPgm AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oplFound AS LOG NO-UNDO.
DEF VAR lcPgmList AS CHAR NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.

oplFound = NO.
hProc = SESSION:FIRST-PROCEDURE.

DO WHILE VALID-HANDLE(hProc):
 
    IF index(hProc:FILE-NAME, ipcPgm) GT 0 THEN
        LEAVE. /* found it. */
    hProc = hProc:NEXT-SIBLING.
END.

IF VALID-HANDLE(hProc) THEN
  oplFound = TRUE.
lcPgmList = PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "").
IF INDEX(lcPgmList, ipcPgm) GT 0 THEN
    oplFound = TRUE.


END.
