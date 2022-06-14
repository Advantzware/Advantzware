DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiRunNo AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipdtNewDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipExecute AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipFilePath AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttGLHist LIKE glhist.

DEFINE VARIABLE hdOutput     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lError       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.

RUN system\OutputProcs.p PERSISTENT SET hdOutput.

{sys/inc/var.i new shared }

DEFINE BUFFER bff-glhist FOR glhist.

ASSIGN
    cocode = ipcCompany.
    
    FIND FIRST period NO-LOCK
             WHERE period.company EQ ipcCompany
             AND period.pst LE ipdtNewDate
             AND period.pend GE ipdtNewDate NO-ERROR.

    FOR EACH bff-glhist EXCLUSIVE-LOCK
        WHERE bff-glhist.company EQ ipcCompany
        AND bff-glhist.tr-num EQ ipiRunNo
        :
        CREATE ttGLHist.
        BUFFER-COPY bff-glhist TO ttGLHist.
         
        IF ipExecute AND ipdtNewDate NE ? THEN
        DO:
            ASSIGN
                bff-glhist.tr-date =  ipdtNewDate
                bff-glhist.period  =  IF AVAIL period THEN period.pnum ELSE MONTH(ipdtNewDate)
                bff-glhist.glYear  =  IF AVAIL period THEN period.yr ELSE YEAR(ipdtNewDate)
                bff-glhist.yr      =  YEAR(ipdtNewDate)
                .            
        END.
        
    END. /*bff-glhist*/
    RELEASE bff-glhist.   

    IF NOT ipExecute THEN
    RUN Output_TempTableToCSV IN hdOutput (TEMP-TABLE ttGLHist:HANDLE, ipFilePath ,YES,YES, OUTPUT lError, OUTPUT cMessage).

THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdOutput).
DELETE OBJECT hdOutput. 
