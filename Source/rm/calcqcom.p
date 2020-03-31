
DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER op-q-com LIKE item.q-comm NO-UNDO.

DEFINE VARIABLE v           AS   INTEGER        NO-UNDO.
DEFINE VARIABLE v-j-no      LIKE job-mat.j-no   NO-UNDO.
DEFINE VARIABLE v-comm      AS   DECIMAL        NO-UNDO.
DEFINE VARIABLE v-bwt       LIKE item.basis-w   NO-UNDO.
DEFINE VARIABLE v-len       LIKE item.s-len     NO-UNDO.
DEFINE VARIABLE v-wid       LIKE item.s-wid     NO-UNDO.
DEFINE VARIABLE v-hld-qty   AS   DECIMAL        NO-UNDO.
DEFINE VARIABLE lRunFromTrigger AS LOG          NO-UNDO.

/* Determine if current program is running from job-mat.p trigger */
RUN get-run-from (INPUT "job-mat", OUTPUT lRunFromTrigger).
/* If this is running from a trigger, don't allow the trigger to execute again */
/* to avoid a cycle in procedure calls                                         */
IF lRunFromTrigger THEN
    DISABLE TRIGGERS FOR LOAD OF job-mat.

FIND item WHERE ROWID(item) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAILABLE item AND item.i-code EQ "R" THEN FOR EACH job-mat EXCLUSIVE WHERE 
    job-mat.company EQ item.company AND 
    job-mat.rm-i-no EQ item.i-no AND 
    job-mat.all-flg EQ YES:

    IF NOT CAN-FIND(FIRST job WHERE 
                    job.company EQ job-mat.company AND 
                    job.opened  EQ YES AND 
                    job.job     EQ job-mat.job AND 
                    job.job-no  EQ job-mat.job-no AND 
                    job.job-no2 EQ job-mat.job-no2) THEN NEXT.

    job-mat.qty-iss = 0.

    FOR EACH mat-act NO-LOCK WHERE 
        mat-act.company EQ item.company AND 
        mat-act.job     EQ job-mat.job AND 
        mat-act.job-no  EQ job-mat.job-no AND 
        mat-act.job-no2 EQ job-mat.job-no2 AND 
        mat-act.s-num   EQ job-mat.frm AND 
        (mat-act.b-num  EQ job-mat.blank-no OR job-mat.blank-no EQ 0) AND 
        mat-act.i-no    EQ job-mat.i-no:

        v-hld-qty = mat-act.qty.

        IF job-mat.qty-uom EQ "EA" THEN DO:
            {sys/inc/roundup.i v-hld-qty}
        END.

        job-mat.qty-iss = job-mat.qty-iss + v-hld-qty.
    END.

    IF job-mat.qty-all LT 0 THEN job-mat.qty-all = 0.
    IF job-mat.qty-all EQ 0 THEN job-mat.all-flg = NO.
    IF NOT job-mat.all-flg THEN NEXT.

    v-comm = job-mat.qty-all.

    IF job-mat.qty-uom NE item.cons-uom THEN DO:
        ASSIGN
            v-bwt = job-mat.basis-w
            v-len = job-mat.len
            v-wid = job-mat.wid.

        IF v-len EQ 0 THEN v-len = item.s-len.

        IF v-wid EQ 0 THEN
            v-wid = IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid.

        IF v-bwt EQ 0 THEN v-bwt = item.basis-w.

        RUN sys/ref/convquom.p(job-mat.qty-uom, item.cons-uom,
                                v-bwt, v-len, v-wid, item.s-dep,
                                v-comm, OUTPUT v-comm).
    END.

    IF v-comm GT 0 THEN op-q-com = op-q-com + v-comm.

END.


PROCEDURE get-run-from:
    DEFINE INPUT PARAMETER ipcPgm AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOG NO-UNDO.
    DEFINE VARIABLE lcPgmList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hProc AS HANDLE NO-UNDO.

    oplFound = NO.
    hProc = SESSION:FIRST-PROCEDURE.

    DO WHILE VALID-HANDLE(hProc):
        IF INDEX(hProc:FILE-NAME, ipcPgm) GT 0 THEN
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
