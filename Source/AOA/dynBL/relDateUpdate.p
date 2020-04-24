/*------------------------------------------------------------------------
  File:         relDateUpdate.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 4.23.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttTempTable
DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD orderNo      AS INTEGER   FORMAT ">>>>>>9"    LABEL "Order"
    FIELD dueDate      AS DATE      FORMAT "99/99/9999" LABEL "Due Date"
    FIELD newDueDate   AS DATE      FORMAT "99/99/9999" LABEL "New Date"
    FIELD lineNo       AS INTEGER   FORMAT ">>>9"       LABEL "Line"
    FIELD itemNo       AS CHARACTER FORMAT "x(40)"      LABEL "Item"
    FIELD reqDate      AS DATE      FORMAT "99/99/9999" LABEL "Req Date"
    FIELD newReqDate   AS DATE      FORMAT "99/99/9999" LABEL "New Date"
    FIELD relNo        AS INTEGER   FORMAT ">>>>>>9"    LABEL "Release"
    FIELD relDate      AS DATE      FORMAT "99/99/9999" LABEL "Rel Date"
    FIELD newRelDate   AS DATE      FORMAT "99/99/9999" LABEL "New Date"
    FIELD schedDate    AS DATE      FORMAT "99/99/9999" LABEL "Sched Date"
    FIELD newSchedDate AS DATE      FORMAT "99/99/9999" LABEL "New Date"
    FIELD sortOrder    AS INTEGER   FORMAT ">>>9"       LABEL "Sort"
        INDEX ttTempTable IS PRIMARY
            orderNo
            sortOrder
            .
DEFINE TEMP-TABLE ttOrder NO-UNDO
    FIELD orderNo AS INTEGER
    FIELD newDate AS DATE
        INDEX ttOrder IS PRIMARY
            orderNo
            .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 102
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx AS INTEGER NO-UNDO.

    IF lUseImportFile THEN
    RUN pLoadImportFile.
    ELSE
    IF lAllReleases EQ NO THEN
    RUN pFindOrdersFromReleases.
    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no GE cStartCustNo
          AND cust.cust-no LE cEndCustNo
        :
        FOR EACH oe-ord EXCLUSIVE-LOCK
            WHERE oe-ord.company EQ cust.company
              AND oe-ord.cust-no EQ cust.cust-no
              AND oe-ord.ord-no  GE iStartOrderNo
              AND oe-ord.ord-no  LE iEndOrderNo
            :
            idx = 0.
            IF CAN-FIND(FIRST ttOrder) THEN DO:
                FIND FIRST ttOrder
                     WHERE ttOrder.orderNo EQ oe-ord.ord-no
                     NO-ERROR.
                IF NOT AVAILABLE ttOrder THEN NEXT.
                dtAsOfDate = ttOrder.newDate.
            END. /* if luseimportfile */
            IF lOrderHeaderDueDate THEN DO:
                idx = idx + 1.
                RUN pUpdateTempRecord (
                    "OrderHeader",
                    oe-ord.ord-no,
                    oe-ord.due-date,
                    0,
                    "",
                    ?,
                    0,
                    ?,
                    ?,
                    dtAsOfDate,
                    idx
                    ).
                IF lPost THEN
                oe-ord.due-date = dtAsOfDate.
            END. /* if lorderheadduedate */
            IF lOrderLineDueDate THEN DO:
                idx = 0.
                FOR EACH oe-ordl EXCLUSIVE-LOCK
                    WHERE oe-ordl.company EQ oe-ord.company
                      AND oe-ordl.ord-no  EQ oe-ord.ord-no
                    :
                    idx = idx + 1.
                    RUN pUpdateTempRecord (
                        "OrderLine",
                        oe-ord.ord-no,
                        ?,
                        oe-ordl.line,
                        oe-ordl.i-no,
                        oe-ordl.req-date,
                        0,
                        ?,
                        ?,
                        dtAsOfDate,
                        idx
                        ).
                    IF lPost THEN
                    oe-ordl.req-date = dtAsOfDate.
                END. /* each oe-ordl */
            END. /* if lorderduedate */
            idx = 0.
            IF lActualReleaseDate OR lScheduledReleaseDate THEN
            FOR EACH oe-rel EXCLUSIVE-LOCK
                WHERE oe-rel.company EQ oe-ord.company
                  AND oe-rel.ord-no  EQ oe-ord.ord-no
                :
                IF lActualReleaseDate THEN DO:
                    FIND FIRST oe-relh EXCLUSIVE-LOCK
                         WHERE oe-relh.r-no     EQ oe-rel.r-no
                           AND oe-relh.release# GE iStartRelease
                           AND oe-relh.release# LE iEndRelease
                         NO-ERROR.
                    IF AVAILABLE oe-relh THEN DO:
                        jdx = jdx + 1.
                        RUN pUpdateTempRecord (
                            "Actual",
                            oe-ord.ord-no,
                            ?,
                            0,
                            "",
                            ?,
                            oe-relh.release#,
                            oe-relh.rel-date,
                            ?,
                            dtAsOfDate,
                            jdx
                            ).
                        IF lPost THEN
                        oe-relh.rel-date = dtAsOfDate.
                    END. /* if avail */
                END. /* lactualreleasedate */
                IF lScheduledReleaseDate THEN DO:
                    idx = idx + 1.
                    RUN pUpdateTempRecord (
                        "Scheduled",
                        oe-ord.ord-no,
                        ?,
                        0,
                        oe-rel.i-no,
                        oe-rel.line,
                        IF AVAILABLE oe-relh THEN oe-relh.release# ELSE ?,
                        ?,
                        oe-rel.rel-date,
                        dtAsOfDate,
                        idx
                        ).
                    IF lPost THEN
                    oe-rel.rel-date  = dtAsOfDate.
                END. /* lscheduledreleasedate */
            END. /* each oe-rel */
        END. /* each oe-ord */
    END. /* each cust */
END PROCEDURE.

PROCEDURE pFindOrdersFromReleases:
    FOR EACH oe-relh NO-LOCK
        WHERE oe-relh.company  EQ cCompany
          AND oe-relh.release# GE iStartRelease
          AND oe-relh.release# LE iEndRelease
        :
        FIND FIRST oe-rel NO-LOCK
             WHERE oe-rel.company EQ oe-relh.company
               AND oe-rel.r-no    EQ oe-relh.r-no
               AND oe-rel.ord-no  GT 0
             NO-ERROR.
        IF NOT AVAILABLE oe-rel THEN NEXT.
        IF CAN-FIND(FIRST ttOrder
                    WHERE ttOrder.orderNo EQ oe-rel.ord-no) THEN
        NEXT.
        CREATE ttOrder.
        ASSIGN
            ttOrder.orderNo = oe-rel.ord-no
            ttOrder.newDate = dtAsOfDate
            .
    END. /* each oe-relh */
END PROCEDURE.

PROCEDURE pLoadImportFile:
    DEFINE VARIABLE iKey   AS INTEGER NO-UNDO.
    DEFINE VARIABLE dtDate AS DATE    NO-UNDO.

    IF SEARCH(cImportFile) NE ? THEN DO:
        ASSIGN
            iStartOrderNo = 9999999
            iEndOrderNo   = 0
            iStartRelease = IF cImportType EQ "Releases" THEN 9999999 ELSE 0
            iEndRelease   = IF cImportType EQ "Releases" THEN 0 ELSE 9999999
            .
        INPUT FROM VALUE(SEARCH(cImportFile)) NO-ECHO.
        REPEAT:
            IMPORT iKey dtDate.
            IF cImportType EQ "Releases" THEN DO:
                FIND FIRST oe-relh NO-LOCK
                     WHERE oe-relh.company  EQ cCompany
                       AND oe-relh.release# EQ iKey
                     NO-ERROR.
                IF NOT AVAILABLE oe-relh THEN NEXT.
                FIND FIRST oe-rel NO-LOCK
                     WHERE oe-rel.company EQ oe-relh.company
                       AND oe-rel.r-no    EQ oe-relh.r-no
                       AND oe-rel.ord-no  GT 0
                     NO-ERROR.
                IF NOT AVAILABLE oe-rel THEN NEXT.
                IF iStartRelease GT iKey THEN
                iStartRelease = iKey.
                IF iEndRelease LT iKey THEN
                iEndRelease = iKey.
                iKey = oe-rel.ord-no.
            END. /* if releases */
            IF iStartOrderNo GT iKey THEN
            iStartOrderNo = iKey.
            IF iEndOrderNo LT iKey THEN
            iEndOrderNo = iKey.
            CREATE ttOrder.
            ASSIGN
                ttOrder.orderNo = iKey
                ttOrder.newDate = dtDate
                .
        END. /* repeat */
        INPUT CLOSE.
    END. /* if search */
END PROCEDURE.

PROCEDURE pUpdateTempRecord:
    DEFINE INPUT PARAMETER ipcType       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrderNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDueDate   AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipiLineNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtReqDate   AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipiRelNo      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipdtRelDate   AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipdtSchedDate AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOfDate  AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipiSortOrder  AS INTEGER   NO-UNDO.

    FIND FIRST ttTempTable
         WHERE ttTempTable.orderNo   EQ ipiOrderNo
           AND ttTempTable.sortOrder EQ ipiSortOrder
         NO-ERROR.
    IF NOT AVAILABLE ttTempTable THEN DO:
        CREATE ttTempTable.
        ASSIGN
            ttTempTable.orderNo   = ipiOrderNo 
            ttTempTable.sortOrder = ipiSortOrder
            .
    END. /* if not avail */

    CASE ipcType:
        WHEN "OrderHeader" THEN DO:
            ASSIGN
                ttTempTable.dueDate    = ipdtDueDate
                ttTempTable.newDueDate = ipdtAsOfDate
                .
        END. /* order */
        WHEN "OrderLine" THEN DO:
            ASSIGN
                ttTempTable.lineNo     = ipiLineNo  
                ttTempTable.itemNo     = ipcItemNo  
                ttTempTable.reqDate    = ipdtReqDate
                ttTempTable.newReqDate = ipdtAsOfDate
                .
        END. /* order */
        WHEN "Actual" THEN DO:
            ASSIGN
                ttTempTable.relNo      = ipiRelNo
                ttTempTable.relDate    = ipdtRelDate
                ttTempTable.newRelDate = ipdtAsOfDate
                .
        END. /* order */
        WHEN "Scheduled" THEN DO:
            IF ttTempTable.itemNo EQ "" THEN
            ASSIGN
                ttTempTable.lineNo = ipiLineNo
                ttTempTable.itemNo = ipcItemNo
                .
            ASSIGN
                ttTempTable.schedDate    = ipdtSchedDate
                ttTempTable.newSchedDate = ipdtAsOfDate
                .
        END. /* order */
    END CASE.
END PROCEDURE.
