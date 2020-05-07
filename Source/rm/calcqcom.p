/* Changes here (05/05/20 - MYT) are mostly cosmetic to better understand the program
    flow.  There was a question as to whether the lRunFromTrigger was processing correctly
    based on current session tree, but simple tests appeared to indicate that this was
    being done.  In either case, added a test for "is my calling program the job-mat trigger?".
    Additionally, made changes in the job-mat trigger that eliminate the call to this program
    completely, pending further testing. */
    
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-q-com LIKE item.q-comm NO-UNDO.

DEF VAR v           AS   INT                                            NO-UNDO.
DEF VAR v-j-no      LIKE job-mat.j-no                                   NO-UNDO.
DEF VAR v-comm      AS   DEC                                            NO-UNDO.
DEF VAR v-bwt       LIKE item.basis-w                                   NO-UNDO.
DEF VAR v-len       LIKE item.s-len                                     NO-UNDO.
DEF VAR v-wid       LIKE item.s-wid                                     NO-UNDO.
DEF VAR v-hld-qty   AS   DEC                                            NO-UNDO.
DEF VAR v-iss-qty   AS DEC NO-UNDO.
DEF VAR lRunFromTrigger AS LOG                                          NO-UNDO.

/* Determine if current program is running from job-mat.p trigger */
RUN get-run-from (INPUT "job-mat", OUTPUT lRunFromTrigger).

/* If this is running from a trigger, don't allow the trigger to execute again */
/* to avoid a cycle in procedure calls                                         */
IF lRunFromTrigger 
OR PROGRAM-NAME(2) EQ "write.trg/job-mat.p" THEN
    DISABLE TRIGGERS FOR LOAD OF job-mat.

FIND item WHERE ROWID(item) EQ ip-rowid NO-LOCK NO-ERROR.

/* Rebuilds the item.q-comm value bases on a re-read of every job-mat and mat-act that are open 
    and reference this item.  */
IF AVAIL item 
AND item.i-code EQ "R" THEN FOR EACH job NO-LOCK WHERE 
    job.company EQ item.company AND 
    job.opened  EQ YES,
    EACH job-mat EXCLUSIVE WHERE 
        job-mat.company EQ job.company AND 
        job-mat.job     EQ job.job AND 
        job-mat.job-no  EQ job.job-no AND 
        job-mat.job-no2 EQ job.job-no2 AND 
        job-mat.rm-i-no EQ item.i-no AND 
        job-mat.all-flg EQ YES:

    ASSIGN 
        v-iss-qty = 0.

    FOR EACH mat-act NO-LOCK WHERE 
        mat-act.company EQ item.company AND  
        mat-act.job     EQ job-mat.job AND 
        mat-act.job-no  EQ job-mat.job-no AND  
        mat-act.job-no2 EQ job-mat.job-no2 AND  
        mat-act.s-num   EQ job-mat.frm AND 
        (mat-act.b-num  EQ job-mat.blank-no OR job-mat.blank-no EQ 0) AND  
        mat-act.i-no    EQ job-mat.i-no:
        ASSIGN 
            v-hld-qty = mat-act.qty.
        IF job-mat.qty-uom EQ "EA" THEN DO:
            {sys/inc/roundup.i v-hld-qty}
        END.
        ASSIGN 
            v-iss-qty = v-iss-qty + v-hld-qty.
    END.
    
    ASSIGN 
        job-mat.qty-iss = v-iss-qty
        job-mat.qty-all = IF job-mat.qty-all LT 0 THEN 0 ELSE job-mat.qty-all
        job-mat.all-flg = IF job-mat.qty-all EQ 0 THEN NO ELSE job-mat.all-flg. 

    IF NOT job-mat.all-flg THEN 
        NEXT.

    ASSIGN 
        v-comm = job-mat.qty-all.

    IF job-mat.qty-uom NE item.cons-uom THEN DO:
        ASSIGN 
            v-bwt = job-mat.basis-w
            v-len = job-mat.len
            v-wid = job-mat.wid
            v-len = IF v-len EQ 0 THEN ITEM.s-len ELSE v-len
            v-wid = IF v-wid EQ 0 THEN (IF ITEM.r-wid NE 0 THEN ITEM.r-wid ELSE ITEM.s-wid) ELSE v-wid
            v-bwt = IF v-bwt EQ 0 THEN ITEM.basis-w ELSE v-bwt.

        RUN sys/ref/convquom.p (job-mat.qty-uom, 
                                item.cons-uom,
                                v-bwt, 
                                v-len, 
                                v-wid, 
                                item.s-dep,
                                v-comm, 
                                OUTPUT v-comm).
    END.

    IF v-comm GT 0 THEN ASSIGN 
        op-q-com = op-q-com + v-comm.
END.


PROCEDURE get-run-from:
    DEF INPUT PARAMETER ipcPgm AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplFound AS LOG NO-UNDO.
    DEF VAR lcPgmList AS CHAR NO-UNDO.
    DEFINE VARIABLE hProc AS HANDLE NO-UNDO.

    ASSIGN 
        oplFound = NO
        hProc = SESSION:FIRST-PROCEDURE.

    DO WHILE VALID-HANDLE(hProc):
        IF INDEX(hProc:FILE-NAME, ipcPgm) GT 0 THEN
            LEAVE. /* found it. */
        hProc = hProc:NEXT-SIBLING.
    END.

    IF VALID-HANDLE(hProc) THEN
        oplFound = TRUE.
    ASSIGN 
        lcPgmList = PROGRAM-NAME(1) + 
        (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "") + 
        (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "") + 
        (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "") + 
        (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "") + 
        (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "") + 
        (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "").
    IF INDEX(lcPgmList, ipcPgm) GT 0 THEN
        oplFound = TRUE.
END.
