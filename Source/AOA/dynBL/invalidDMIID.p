/*------------------------------------------------------------------------
  File:         invalidDMIID.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 2.23.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttDMITrans
DEFINE TEMP-TABLE ttDMITrans NO-UNDO LIKE dmiTrans.

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 163
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cEstimateID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTag        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lExportOnly AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bDMITrans FOR dmiTrans.

    EMPTY TEMP-TABLE ttDMITrans.
    FOR EACH dmiTrans NO-LOCK
        WHERE dmiTrans.posted EQ NO
           BY dmiTrans.dmiID
           BY dmiTrans.startDate
           BY dmiTrans.startTime
        :
        idx = idx + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, idx, ?).
        IF CAN-FIND(FIRST mach
                    WHERE mach.spare-int-2 EQ dmiTrans.dmiID) THEN
        NEXT.
        // only process dmitrans record with invalid dmiid values
        CASE cPostDelete:
            WHEN "Delete" THEN DO:
                CREATE ttDMITrans.
                BUFFER-COPY dmiTrans TO ttDMITrans.
                DELETE dmiTrans.
            END. /* delete */
            WHEN "Post" THEN DO:
                // set these values when known
                ASSIGN
                    cEstimateID = ?
                    cTag        = ?
                    lExportOnly = ?
                    .
                RUN jc\ProcessFurnishBatch.p (
                    cCompany,
                    cEstimateID,
                    cTag,
                    lExportOnly,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).
                DO TRANSACTION:
                    FIND FIRST bDMITrans EXCLUSIVE-LOCK
                         WHERE ROWID(bDMITrans) EQ ROWID(dmiTrans).
                    bDMITrans.posted = YES.
                END. /* do trans */
            END. /* post */
        END CASE.
    END. /* each dmitrans */

END PROCEDURE.
