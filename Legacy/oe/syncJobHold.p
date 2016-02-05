DEF INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiOrdNo   AS INTEGER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcNewStat AS CHARACTER NO-UNDO. /* "open" or "hold" or one character status */
DEFINE VARIABLE cNewStat      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRtnChar      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE OeJobHold-log AS LOGICAL   NO-UNDO.
FIND FIRST oe-ord
    WHERE oe-ord.company EQ ipcCompany
    AND oe-ord.ord-no EQ ipiOrdNo
    NO-LOCK NO-ERROR.
IF AVAIL oe-ord THEN 
RUN sys/ref/nk1look.p (oe-ord.company, "OEJobHold", "L", NO, NO, "", "", 
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    OeJobHold-log = LOGICAL(cRtnChar) NO-ERROR.
IF oeJobHold-log THEN 
DO:
    cNewStat = ipcNewStat.
    IF LENGTH(cNewStat) GT 1 THEN 
    DO:
        IF cNewStat BEGINS "Open" OR cNewStat BEGINS "Release" OR cNewStat BEGINS "Approve"
            THEN cNewStat = "R".
        ELSE IF cNewStat BEGINS "H" THEN
                cNewStat = "H".
    END.
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ ipcCompany
        AND oe-ord.ord-no EQ ipiOrdNo
        NO-LOCK NO-ERROR.
  
    IF AVAIL oe-ord THEN 
    DO:
    
    
        IF oe-ord.job-no GT "" THEN 
        DO:
        
            FIND FIRST job 
                WHERE job.company EQ oe-ord.company
                AND   job.job-no  EQ oe-ord.job-no
                AND   job.job-no2 EQ oe-ord.job-no2
                EXCLUSIVE-LOCK NO-ERROR.
          
            IF AVAIL job THEN 
            DO:
                job.stat = cNewStat.
                FIND CURRENT job NO-LOCK.
            END.
            RELEASE job.
        END.
    
        FOR EACH oe-ordl 
            WHERE oe-ordl.company EQ oe-ord.company
            AND oe-ordl.ord-no  EQ oe-ord.ord-no
            NO-LOCK:
          
            FIND FIRST job 
                WHERE job.company EQ oe-ordl.company
                AND   job.job-no  EQ oe-ordl.job-no
                AND   job.job-no2 EQ oe-ordl.job-no2
                EXCLUSIVE-LOCK NO-ERROR.
          
            IF AVAIL job THEN 
            DO:
                job.stat = cNewStat.
                FIND CURRENT job NO-LOCK.
            END.
            RELEASE job.
            
        END.
    END.
END.
