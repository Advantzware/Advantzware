/*------------------------------------------------------------------------
  File:         ttOEEEfficiency.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 6.30.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttOEEEfficiency
DEFINE TEMP-TABLE ttOEEEfficiency NO-UNDO LIKE dmiTrans
    FIELD itemNo       AS CHARACTER FORMAT "x(15)"          LABEL "Product"
    FIELD runTimeCalc  AS DECIMAL   FORMAT "->>>,>>9.9<<<<" LABEL "Run Time"
    FIELD downtimeCalc AS DECIMAL   FORMAT "->>>,>>9.9<<<<" LABEL "Downtime"
    FIELD recFound     AS LOGICAL                           LABEL "Found"
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 210
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cItemNo         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dTimeCalc       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hDynCalcField   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iJobMachID      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iShiftStartTime AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iShiftEndTime   AS INTEGER   NO-UNDO INITIAL 86400000.

    RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.
    SESSION:ADD-SUPER-PROCEDURE (hDynCalcField).

    RUN calcShiftStartTime (
        cCompany,
        lUseTimes,
        cStartShift,
        cStartTime,
        OUTPUT iShiftStartTime
        ).
    RUN calcShiftEndTime (
        cCompany,
        lUseTimes,
        cStartShift,
        cEndShift,
        cEndTime,
        OUTPUT iShiftEndTime
        ).
    IF dtStartTransDate EQ dtEndTransDate  AND
       iShiftEndTime    LT iShiftStartTime THEN
    iShiftEndTime = 86400000.

    FOR EACH dmiTrans NO-LOCK
        WHERE dmiTrans.dmiID GE iStartDMIID
          AND dmiTrans.dmiID LE iEndDMIID
          AND dmiTrans.shift GE cStartShift
          AND dmiTrans.shift LE cEndShift
          AND DATETIME(dmiTrans.startDate, dmiTrans.startTime * 1000) GE DATETIME(dtStartTransDate, iShiftStartTime)
          AND DATETIME(dmiTrans.tranDate,  dmiTrans.tranTime  * 1000) LE DATETIME(dtEndTransDate,   iShiftEndTime)
          AND (cPostedType   EQ "All" OR dmiTrans.posted     EQ (cPostedType EQ "yes"))
          AND (cDMIStateType EQ "All" OR dmiTrans.transState EQ cDMIStateType)
        :
        cItemNo = "".
        RELEASE job-mch.
        IF NUM-ENTRIES(dmiTrans.jobID,".") GT 1 THEN DO:
            ASSIGN
                iJobMachID = INTEGER(ENTRY(2,dmiTrans.jobID,"."))
                cJobNo     = ENTRY(1,dmiTrans.jobID,".")
                iJobNo2    = INTEGER(ENTRY(2,cJobNo,"-"))
                cJobNo     = ENTRY(1,cJobNo,"-")
                cJobNo     = FILL(" ",6 - LENGTH(cJobNo))
                .
            FIND FIRST job-mch NO-LOCK
                 WHERE job-mch.job-mchID EQ iJobMachID
                 NO-ERROR.
            IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch NO-LOCK USE-INDEX start-date
                 WHERE job-mch.company EQ cCompany
                   AND job-mch.job-no  EQ cJobNo
                   AND job-mch.job-no2 EQ iJobNo2
                 NO-ERROR.
            IF AVAILABLE job-mch THEN DO:
                cItemNo = job-mch.i-no.
                IF cItemNo EQ "" THEN DO:
                    FIND FIRST job-hdr NO-LOCK
                         WHERE job-hdr.company   EQ job-mch.company
                           AND job-hdr.job-no    EQ job-mch.job-no
                           AND job-hdr.job-no2   EQ job-mch.job-no2
                           AND job-hdr.frm       EQ job-mch.frm
                           AND (job-hdr.blank-no EQ job-mch.blank-no
                            OR job-mch.blank-no  EQ 0)
                         NO-ERROR.
                    IF AVAILABLE job-hdr THEN
                    cItemNo = job-hdr.i-no.
                END. // if citemno eq blank
            END. // avail job-mch
        END. // if num-entries
        CREATE ttOEEEfficiency.
        BUFFER-COPY dmiTrans TO ttOEEEfficiency.
        ASSIGN
            ttOEEEfficiency.itemNo   = cItemNo
            ttOEEEfficiency.recFound = AVAILABLE(job-mch)
            dTimeCalc = DYNAMIC-FUNCTION("fTimeSpan",
                        dmiTrans.startDate,
                        dmiTrans.startTime,
                        dmiTrans.tranDate,
                        dmiTrans.tranTime
                        )
            .
        IF dmiTrans.transState EQ "DT" THEN
        ttOEEEfficiency.downtimeCalc = dTimeCalc.
        ELSE
        ttOEEEfficiency.runTimeCalc = dTimeCalc.
    END. // each dmitran

    IF VALID-HANDLE(hDynCalcField) THEN
    DELETE PROCEDURE hDynCalcField.

END PROCEDURE.
