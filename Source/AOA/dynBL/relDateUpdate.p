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
DEFINE TEMP-TABLE ttPost NO-UNDO
    FIELD tableName  AS CHARACTER
    FIELD tableRowID AS ROWID
    FIELD fieldDate  AS DATE
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
          AND cust.active  NE "I"
        :
        FOR EACH oe-ord NO-LOCK
            WHERE oe-ord.company EQ cust.company
              AND oe-ord.cust-no EQ cust.cust-no
              AND oe-ord.ord-no  GE iStartOrderNo
              AND oe-ord.ord-no  LE iEndOrderNo
              AND oe-ord.opened  EQ YES
              AND oe-ord.deleted EQ NO
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
                RUN pCreatePost ("oe-ord", ROWID(oe-ord), dtAsOfDate).
            END. /* if lorderheadduedate */
            IF lOrderLineDueDate THEN DO:
                idx = 0.
                FOR EACH oe-ordl NO-LOCK
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
                    RUN pCreatePost ("oe-ordl", ROWID(oe-ordl), dtAsOfDate).
                END. /* each oe-ordl */
            END. /* if lorderduedate */
            idx = 0.
            IF lScheduledReleaseDate THEN
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-ord.company
                  AND oe-rel.ord-no  EQ oe-ord.ord-no
                  AND oe-rel.deleted EQ NO
                :
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
                RUN pCreatePost ("oe-rel", ROWID(oe-rel), dtAsOfDate).
            END. /* each oe-rel */
            IF lActualReleaseDate THEN
            FOR EACH oe-rell NO-LOCK
                WHERE oe-rell.company EQ oe-ord.company
                  AND oe-rell.ord-no  EQ oe-ord.ord-no
                  AND oe-rell.deleted EQ NO
                  AND oe-rell.posted  EQ NO
                :
                FIND FIRST oe-relh NO-LOCK
                     WHERE oe-relh.r-no     EQ oe-rell.r-no
                       AND oe-relh.release# GE iStartRelease
                       AND oe-relh.release# LE iEndRelease
                       AND oe-relh.deleted  EQ NO
                       AND oe-relh.posted   EQ NO
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
                    RUN pCreatePost ("oe-relh", ROWID(oe-relh), dtAsOfDate).
                END. /* if avail */
            END. /* each oe-rell */
        END. /* each oe-ord */
    END. /* each cust */
    IF lPost THEN
    RUN pPostDates.
END PROCEDURE.

PROCEDURE pCreatePost:
    DEFINE INPUT PARAMETER ipcTableName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iprTableRowID AS ROWID     NO-UNDO.
    DEFINE INPUT PARAMETER ipdtFieldDate AS DATE      NO-UNDO.
    
    CREATE ttPost.
    ASSIGN
        ttPost.tableName  = ipcTableName
        ttPost.tableRowID = iprTableRowID
        ttPost.fieldDate  = ipdtFieldDate
        .
END PROCEDURE.

PROCEDURE pFindOrdersFromReleases:
    FOR EACH oe-relh NO-LOCK
        WHERE oe-relh.company  EQ cCompany
          AND oe-relh.release# GE iStartRelease
          AND oe-relh.release# LE iEndRelease
          AND oe-relh.deleted  EQ NO
          AND oe-relh.posted   EQ NO
        :
        FIND FIRST oe-rell NO-LOCK
             WHERE oe-rell.company EQ oe-relh.company
               AND oe-rell.r-no    EQ oe-relh.r-no
               AND oe-rell.ord-no  GT 0
               AND oe-rell.deleted EQ NO
               AND oe-rell.posted  EQ NO
             NO-ERROR.
        IF NOT AVAILABLE oe-rell THEN NEXT.
        IF CAN-FIND(FIRST ttOrder
                    WHERE ttOrder.orderNo EQ oe-rell.ord-no) THEN
        NEXT.
        CREATE ttOrder.
        ASSIGN
            ttOrder.orderNo = oe-rell.ord-no
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
                FIND FIRST oe-rell NO-LOCK
                     WHERE oe-rell.company EQ oe-relh.company
                       AND oe-rell.r-no    EQ oe-relh.r-no
                       AND oe-rell.ord-no  GT 0
                       AND oe-rell.deleted EQ NO
                       AND oe-rell.posted  EQ NO
                     NO-ERROR.
                IF NOT AVAILABLE oe-rell THEN NEXT.
                IF iStartRelease GT iKey THEN
                iStartRelease = iKey.
                IF iEndRelease LT iKey THEN
                iEndRelease = iKey.
                iKey = oe-rell.ord-no.
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

PROCEDURE pPostDates:
    FOR EACH ttPost:
        CASE ttPost.tableName:
            WHEN "oe-ord" THEN DO:
                FIND FIRST oe-ord EXCLUSIVE-LOCK
                     WHERE ROWID(oe-ord) EQ ttPost.tableRowID
                     NO-ERROR.
                IF AVAILABLE oe-ord THEN
                oe-ord.due-date = ttPost.fieldDate.
            END. /* oe-ord */
            WHEN "oe-ordl" THEN DO:
                FIND FIRST oe-ordl EXCLUSIVE-LOCK
                     WHERE ROWID(oe-ordl) EQ ttPost.tableRowID
                     NO-ERROR.
                IF AVAILABLE oe-ordl THEN
                oe-ordl.req-date = ttPost.fieldDate.
            END. /* oe-ordl */
            WHEN "oe-rel" THEN DO:
                FIND FIRST oe-rel EXCLUSIVE-LOCK
                     WHERE ROWID(oe-rel) EQ ttPost.tableRowID
                     NO-ERROR.
                IF AVAILABLE oe-rel THEN
                oe-rel.rel-date = ttPost.fieldDate.
            END. /* oe-rel */
            WHEN "oe-relh" THEN DO:
                FIND FIRST oe-relh EXCLUSIVE-LOCK
                     WHERE ROWID(oe-relh) EQ ttPost.tableRowID
                     NO-ERROR.
                IF AVAILABLE oe-relh THEN
                oe-relh.rel-date = ttPost.fieldDate.
            END. /* oe-relh */
        END CASE.
    END. /* each ttpost */
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
