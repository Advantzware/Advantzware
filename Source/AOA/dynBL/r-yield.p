/*------------------------------------------------------------------------
  File:         r-yield.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 7.15.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&Scoped-define Yield

/* Temp-Table Definitions ---                                           */
&Scoped-define ttTempTable ttYield
DEFINE TEMP-TABLE ttYield NO-UNDO
    {AOA/tempTable/ttFields.i}
    FIELD dmiID   LIKE dmiTrans.dmiID
    FIELD machine   AS CHARACTER FORMAT "x(6)"       LABEL "Machine"
    FIELD itemNo    AS CHARACTER FORMAT "x(15)"      LABEL "FG Item"
    FIELD jobID   LIKE dmiTrans.jobID                LABEL "FG and DMI Job"
    FIELD transDate AS DATE      FORMAT "99/99/9999" LABEL "Date" 
    FIELD dips      AS INTEGER   FORMAT ">>>,>>9"    LABEL "Dips"
    FIELD molds     AS INTEGER   FORMAT ">9"         LABEL "Molds"
    FIELD made      AS INTEGER   FORMAT ">>>,>>9"    LABEL "Made"
    FIELD fg        AS INTEGER   FORMAT ">>>,>>9"    LABEL "FG"
    FIELD yield     AS DECIMAL   FORMAT ">>9.99"     LABEL "Yield Pct"
    .
DEFINE TEMP-TABLE ttDMITrans NO-UNDO
    {AOA/tempTable/ttFields.i}
    FIELD dmiID     LIKE dmiTrans.dmiID
    FIELD itemNo    LIKE ttYield.itemNo
    FIELD shift     LIKE dmiTrans.shift
    FIELD startDate LIKE dmiTrans.startDate       LABEL "Start Date"
    FIELD startTime   AS CHARACTER FORMAT "x(11)" LABEL "Start Time"
    FIELD endDate   LIKE dmiTrans.tranDate        LABEL "End Date"
    FIELD endTime     AS CHARACTER FORMAT "x(11)" LABEL "End Time"
    FIELD transQty  LIKE dmiTrans.tranRunQty      LABEL "Quantity"
    FIELD xxmolds     AS INTEGER   FORMAT ">9"    LABEL "Molds"
    FIELD xxjobID   LIKE dmiTrans.jobID
    FIELD xxfgJobID LIKE dmiTrans.jobID           LABEL "FG Job"
    FIELD xxStartTime AS INTEGER
        INDEX ttDMITrans IS PRIMARY dmiID startDate xxStartTime itemNo
        .

&Scoped-define subjectID 5211
/*&Scoped-define ttTempTable ttFGPostHist*/
{AOA/dynBL/r-fgpstr.i}

RUN spSetSessionParam ("DetailTables", "1").
RUN spSetSessionParam ("DetailHandle1", TEMP-TABLE ttDMITrans:HANDLE).

/*RUN spSetSessionParam ("DetailTables", "2").                            */
/*RUN spSetSessionParam ("DetailHandle2", TEMP-TABLE ttFGPostHist:HANDLE).*/

/*RUN spSetSessionParam ("SummaryTables", "1").                          */
/*RUN spSetSessionParam ("SummaryHandle1", TEMP-TABLE ttDMITrans:HANDLE).*/
/*RUN spSetSessionParam ("SummaryTitle1", "DMI TRansactions").           */

RUN spSetSessionParam ("SummaryTables", "1").
RUN spSetSessionParam ("SummaryHandle1", TEMP-TABLE ttFGPostHist:HANDLE).
RUN spSetSessionParam ("SummaryTitle1", "FG Transactions").

PROCEDURE pDMITrans:
    DEFINE VARIABLE cItemNo         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hDynCalcField   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iJobMachID      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRecordID       AS INTEGER   NO-UNDO.
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
    IF dtStartDate EQ dtEndDate  AND
       iShiftEndTime    LT iShiftStartTime THEN
    iShiftEndTime = 86400000.

    FOR EACH ttFGPostHist
        BREAK BY ttFGPostHist.transDate
              BY ttFGPostHist.itemNo
        :
        IF LAST-OF(ttFGPostHist.itemNo) THEN DO:
            FOR EACH dmiTrans NO-LOCK
                WHERE dmiTrans.dmiID GE iStartDMIID
                  AND dmiTrans.dmiID LE iEndDMIID
                  AND dmiTrans.shift GE cStartShift
                  AND dmiTrans.shift LE cEndShift
                  AND DATETIME(dmiTrans.startDate, dmiTrans.startTime * 1000) GE DATETIME(ttFGPostHist.transDate, iShiftStartTime)
                  AND DATETIME(dmiTrans.tranDate,  dmiTrans.tranTime  * 1000) LE DATETIME(ttFGPostHist.transDate,   iShiftEndTime)
                  AND dmiTrans.transState EQ "RUN"
                :
                RELEASE job-mch.
                RELEASE job-hdr.
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
                    IF AVAILABLE job-mch THEN DO:
                        FIND FIRST job-hdr NO-LOCK
                             WHERE job-hdr.company   EQ job-mch.company
                               AND job-hdr.job-no    EQ job-mch.job-no
                               AND job-hdr.job-no2   EQ job-mch.job-no2
                               AND job-hdr.frm       EQ job-mch.frm
                               AND (job-hdr.blank-no EQ job-mch.blank-no
                                OR job-mch.blank-no  EQ 0)
                             NO-ERROR.
                        IF NOT AVAILABLE job-hdr THEN
                        FIND FIRST job-hdr NO-LOCK
                             WHERE job-hdr.company EQ job-mch.company
                               AND job-hdr.i-no    EQ ttFGPostHist.itemNo
                               AND job-hdr.job-no  EQ cJobNo
                               AND job-hdr.job-no2 EQ iJobNo2
                             NO-ERROR.
                    END. // avail job-mch
                END. // if num-entries
                ELSE
                    FIND FIRST job-hdr NO-LOCK
                         WHERE job-hdr.company EQ cCompany
                           AND job-hdr.i-no    EQ ttFGPostHist.itemNo
                           AND job-hdr.job-no  EQ FILL(" ",6 - LENGTH(dmiTrans.jobID)) + dmiTrans.jobID
                         NO-ERROR.
                IF NOT AVAILABLE job-hdr THEN NEXT.
                CREATE ttDMITrans.
                ASSIGN
                    ttDMITrans.dmiID       = dmiTrans.dmiID
                    ttDMITrans.itemNo      = ttFGPostHist.itemNo
                    ttDMITrans.shift       = dmiTrans.shift
                    ttDMITrans.startDate   = dmiTrans.startDate
                    ttDMITrans.startTime   = STRING(dmiTrans.startTime,"hh:mm:ss am")
                    ttDMITrans.endDate     = dmiTrans.tranDate
                    ttDMITrans.endTime     = STRING(dmiTrans.tranTime,"hh:mm:ss am")
                    //ttDMITrans.cycleTime = dmiTrans.cycleTime
                    ttDMITrans.transQty    = dmiTrans.tranRunQty
                    ttDMITrans.xxmolds     = job-hdr.n-on
                    ttDMITrans.xxjobID     = dmiTrans.jobID
                    ttDMITrans.xxfgJobID   = ttFGPostHist.jobNo
                    ttDMITrans.xxStartTime = dmiTrans.startTime
                    //ttDMITrans.recordID  = ttFGPostHist.recordID
                    .
            END. // each dmitrans
        END. // if last-of
    END. // each ttFGPostHist
    FOR EACH ttDMITrans
        WHERE ttDMITrans.xxmolds NE ?
        BREAK BY ttDMITrans.dmiID
              BY ttDMITrans.startDate
              BY ttDMITrans.itemNo
        :
        IF FIRST-OF(ttDMITrans.itemNo) THEN DO:
            FIND FIRST mach NO-LOCK
                 WHERE mach.company     EQ cCompany
                   AND mach.spare-int-2 EQ ttDMITrans.dmiID
                 NO-ERROR.
            CREATE ttYield.
            ASSIGN
                iRecordID         = iRecordID + 1
                ttYield.recordID  = iRecordID
                ttYield.dmiID     = ttDMITrans.dmiID
                ttYield.machine   = IF AVAILABLE mach THEN mach.m-code ELSE "Missing"
                ttYield.itemNo    = ttDMITrans.itemNo
                ttYield.jobID     = ttDMITrans.xxfgJobID + ", " + ttDMITrans.xxjobID
                ttYield.transDate = ttDMITrans.startDate
                ttYield.molds     = ttDMITrans.xxmolds
                .
        END. // if first-of
        ASSIGN
            ttYield.dips = ttYield.dips + ttDMITrans.transQty
            ttDMITrans.recordID = ttYield.recordID
            .
        IF LAST-OF(ttDMITrans.itemNo) THEN DO:
            FOR EACH ttFGPostHist
                WHERE ttFGPostHist.transDate EQ ttDMITrans.startDate
                  AND ttFGPostHist.itemNo    EQ ttDMITrans.itemNo
                :
                ASSIGN
                    ttYield.fg = ttYield.fg + ttFGPostHist.v-fg-qty
                    ttFGPostHist.recordID = ttYield.recordID
                    .                
            END. // each ttfgposthist
            ASSIGN
                ttYield.made  = ttYield.dips * ttYield.molds
                ttYield.yield = ttYield.fg / ttYield.made * 100
                .
        END. // if first-of
    END. // each ttdmitrans
    IF NOT lShowDetail THEN DO:
        EMPTY TEMP-TABLE ttDMITrans.
        EMPTY TEMP-TABLE ttFGPostHist.
    END. // not show
    ELSE
    FOR EACH ttFGPostHist
        WHERE ttFGPostHist.dmiID LT iStartDMIID
           OR ttFGPostHist.dmiID GT iEndDMIID
        :
        DELETE ttFGPostHist.
    END. // each ttfgposthist

    IF VALID-HANDLE(hDynCalcField) THEN
    DELETE PROCEDURE hDynCalcField.

END PROCEDURE.
