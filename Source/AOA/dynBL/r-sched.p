/*------------------------------------------------------------------------
  File:         r-sched.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 2.15.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttScheduledReleases
{AOA/tempTable/ttScheduledReleases.i}
{AOA/tempTable/ttScheduledReleasesNotes.i}
{AOA/tempTable/ttScheduledReleasesStats.i}

DEFINE TEMP-TABLE ttReport NO-UNDO
    FIELD company    AS CHARACTER 
    FIELD custNo     AS CHARACTER 
    FIELD tableName  AS CHARACTER 
    FIELD tableRecID AS RECID 
        INDEX idx custNo tableName
        .
DEFINE TEMP-TABLE w-ord
    FIELD ord-no LIKE oe-ord.ord-no
    FIELD est-no LIKE oe-ord.est-no
    FIELD onh-qty LIKE itemfg.q-onh
    FIELD cust-no LIKE oe-ord.cust-no
    FIELD cust-name LIKE oe-ord.cust-name
    FIELD part-no LIKE oe-ordl.part-no
    FIELD i-no LIKE oe-ordl.i-no
    FIELD i-name LIKE oe-ordl.i-name
    FIELD qty LIKE oe-ordl.qty
    FIELD cost LIKE oe-ordl.cost
    FIELD price LIKE oe-ordl.price
    FIELD t-price LIKE oe-ordl.t-price FORMAT "->>,>>>,>>9"
    FIELD rel-qty LIKE oe-rel.qty
    FIELD rel-date AS CHARACTER FORMAT "x(10)"
    FIELD job AS CHARACTER FORMAT "x(9)"
    FIELD job-no LIKE oe-ordl.job-no
    FIELD job-no2 LIKE oe-ordl.job-no2
    FIELD rel-no LIKE oe-rel.rel-no
    FIELD ship-id LIKE oe-rel.ship-id
    FIELD po-num LIKE oe-ordl.po-no
    FIELD ord-qty LIKE oe-ordl.qty
    FIELD shp-qty LIKE oe-ordl.ship-qty
    FIELD msf AS DECIMAL FORMAT "->>9.999"
    FIELD component AS INTEGER
    FIELD prom-code LIKE oe-ordl.prom-code FORMAT 'X(5)'
    FIELD last-date LIKE oe-ord.last-date FORMAT "99/99/99"
    FIELD carrier LIKE oe-relh.carrier
    FIELD is-a-component LIKE oe-ordl.is-a-component
    FIELD palls AS INTEGER FORMAT "->>,>>>,>>9"
    FIELD xls-rel-date LIKE oe-rel.rel-date FORMAT "99/99/99"
    FIELD xls-status AS CHARACTER
    FIELD tot-qty LIKE itemfg.q-onh 
    FIELD job-qty AS INTEGER FORMAT ">>>>>>9"
    FIELD v-note1 AS CHARACTER 
    FIELD v-note2 AS CHARACTER
    FIELD v-note3 AS CHARACTER
    FIELD v-note4 AS CHARACTER  
    FIELD ship-from AS CHARACTER  
    FIELD sman AS CHARACTER
    FIELD upd-user AS CHARACTER
    FIELD due-date AS DATE
    FIELD csrUser_id AS CHARACTER
    FIELD cType AS CHARACTER
    FIELD dLastRecDate LIKE fg-rcpth.trans-date
    FIELD entered-id AS CHARACTER 
    FIELD ord-due-date AS CHARACTER
    FIELD rel-due-date AS CHARACTER
    FIELD Printed AS LOGICAL
    FIELD promiseDate AS CHARACTER
    FIELD priority AS INTEGER
    FIELD vend-id AS CHARACTER
    FIELD vend-name AS CHARACTER
    FIELD vend-po AS INTEGER 
    FIELD po-due-date AS DATE
    FIELD po-rm-item AS CHARACTER
    FIELD po-rm-item-name AS CHARACTER
    FIELD po-uom AS CHARACTER
    FIELD po-ord-qty AS DECIMAL
    FIELD po-rec-qty AS DECIMAL
    FIELD pr-uom AS CHARACTER 
    .
DEFINE WORKFILE tt-fg-set LIKE fg-set
    FIELD isaset     LIKE itemfg.isaset
    FIELD alloc      LIKE itemfg.alloc
    FIELD part-qty-dec AS DECIMAL
    .
DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD qty LIKE oe-rell.qty
    .
{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 159
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE iQty             AS INTEGER           NO-UNDO.
    DEFINE VARIABLE cType            AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cTypes           AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE iReleaseNo     LIKE oe-relh.release#  NO-UNDO.
    DEFINE VARIABLE cShipFrom      LIKE oe-rell.loc       NO-UNDO.
    DEFINE VARIABLE dtDate         LIKE oe-relh.rel-date  NO-UNDO.
    DEFINE VARIABLE cPONum         LIKE oe-rell.po-no     NO-UNDO.
    DEFINE VARIABLE cShipID        LIKE oe-relh.ship-id   NO-UNDO.
    DEFINE VARIABLE cCarrier       LIKE oe-relh.carrier   NO-UNDO.
    DEFINE VARIABLE dPallets         AS DECIMAL           NO-UNDO.
    DEFINE VARIABLE iCount           AS INTEGER           NO-UNDO.
    DEFINE VARIABLE iTotal           AS INTEGER           NO-UNDO.
    DEFINE VARIABLE iOHRelQty        AS INTEGER           NO-UNDO.
    DEFINE VARIABLE cCRRate        LIKE cust.cr-rating    NO-UNDO.
    DEFINE VARIABLE cTerritory     LIKE cust.terr         NO-UNDO.
    DEFINE VARIABLE cDelZone       LIKE cust.del-zone     NO-UNDO.
    DEFINE VARIABLE cShipAddr1       AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cShipAddr2       AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cShipCity      LIKE shipto.ship-city  NO-UNDO.
    DEFINE VARIABLE cShipState     LIKE shipto.ship-state NO-UNDO.
    DEFINE VARIABLE cShipZip       LIKE shipto.ship-zip   NO-UNDO.
    DEFINE VARIABLE cShipName      LIKE shipto.ship-name  NO-UNDO.
    DEFINE VARIABLE cReasonCode      AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cReasonDesc      AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE lRunComplete     AS LOGICAL           NO-UNDO.
    DEFINE VARIABLE cRouting         AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cDockHr        LIKE shipto.dock-hour  NO-UNDO.
    DEFINE VARIABLE cReltyp        LIKE oe-rell.s-code    NO-UNDO.
    DEFINE VARIABLE cGetVendorPOInfo AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE lGetVendorPOInfo AS LOGICAL           NO-UNDO.
    DEFINE VARIABLE idx              AS INTEGER           NO-UNDO.
    DEFINE VARIABLE cRelDueDate      AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE lPrinted         AS LOGICAL           NO-UNDO.

    DEFINE BUFFER b-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER b-itemfg   FOR itemfg.
    DEFINE BUFFER bw-ord     FOR w-ord.
    
    ASSIGN
        cTypes           = STRING(lScheduled,"S/")
                         + STRING(lLate,"L/")
                         + STRING(lPastLastShipDate,"I/")
                         + STRING(lActual,"A/")
                         + STRING(lBackorder,"B/")
                         + STRING(lBillOfLading,"P/")
                         + STRING(lInvoiceUnposted,"Z/")
                         + STRING(lCompleted,"C/")
        cGetVendorPOInfo = "ttScheduledReleases.vendorID,"
                         + "ttScheduledReleases.vendorName,"
                         + "ttScheduledReleases.vendorPO,"
                         + "ttScheduledReleases.poDueDate,"
                         + "ttScheduledReleases.poRMItemNo,"
                         + "ttScheduledReleases.poRMItemName,"
                         + "ttScheduledReleases.poOUM,"
                         + "ttScheduledReleases.poOrderQty,"
                         + "ttScheduledReleases.poReceivedQty"
                         .
    DO idx = 1 TO NUM-ENTRIES(cGetVendorPOInfo):
        lGetVendorPOInfo = CAN-FIND(FIRST dynValueColumn
                                    WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
                                      AND dynValueColumn.user-id      EQ dynParamValue.user-id
                                      AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
                                      AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
                                      AND dynValueColumn.colName      EQ ENTRY(idx,cGetVendorPOInfo))
                                      .
        IF lGetVendorPOInfo THEN LEAVE.
    END. /* do idx */

    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company    EQ cCompany
          AND oe-ordl.opened     EQ YES
          AND oe-ordl.ord-no     GE iStartOrderNo
          AND oe-ordl.ord-no     LE iEndOrderNo
          AND oe-ordl.i-no       GE cStartFGItem
          AND oe-ordl.i-no       LE cEndFGItem
          AND ((oe-ordl.s-man[1] GE cStartSalesRep 
          AND   oe-ordl.s-man[1] LE cEndSalesRep)
           OR  (oe-ordl.s-man[2] GE cStartSalesRep
          AND   oe-ordl.s-man[2] LE cEndSalesRep)
           OR  (oe-ordl.s-man[3] GE cStartSalesRep
          AND   oe-ordl.s-man[3] LE cEndSalesRep))
          AND CAN-FIND(FIRST b-oe-ordl
                      {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
                       USE-INDEX ord-no) EQ NO
          USE-INDEX opened,
          FIRST b-itemfg NO-LOCK
          WHERE b-itemfg.company EQ cCompany
            AND b-itemfg.i-no    EQ oe-ordl.i-no
            AND b-itemfg.procat  GE cStartProCat
            AND b-itemfg.procat  LE cEndProCat,
          FIRST oe-ord NO-LOCK 
          WHERE oe-ord.company    EQ oe-ordl.company
            AND oe-ord.ord-no     EQ oe-ordl.ord-no
            AND oe-ord.cust-no    GE cStartCustNo
            AND oe-ord.cust-no    LE cEndCustNo
            AND oe-ord.csrUser_id GE cStartUserID
            AND oe-ord.csrUser_id LE cEndUserID,
          FIRST cust NO-LOCK
          WHERE cust.company EQ oe-ord.company
            AND cust.cust-no EQ oe-ord.cust-no
          :
        IF lCustList AND
           NOT CAN-FIND(FIRST ttCustList
                        WHERE ttCustList.cust-no EQ oe-ord.cust-no
                          AND ttCustList.log-fld EQ TRUE) THEN
        NEXT.
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company      EQ oe-ordl.company
              AND oe-rel.ord-no       EQ oe-ordl.ord-no
              AND oe-rel.i-no         EQ oe-ordl.i-no
              AND oe-rel.line         EQ oe-ordl.line
              AND oe-rel.rel-date     GE dtStartReleaseDate
              AND oe-rel.rel-date     LE dtEndReleaseDate
              AND oe-rel.carrier      GE cStartCarrNo
              AND oe-rel.carrier      LE cEndCarrNo
              AND oe-rel.spare-char-1 GE cStartShipFromNo
              AND oe-rel.spare-char-1 LE cEndShipFromNo
            USE-INDEX ord-item
            :      
            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT cType).
            IF INDEX("AB",cType) GT 0 THEN NEXT.    
            IF INDEX(cTypes,cType) GT 0 THEN DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = IF cSort EQ "Release Date" THEN
                                            STRING(YEAR(oe-rel.rel-date),"9999") +
                                            STRING(MONTH(oe-rel.rel-date),"99")  +
                                            STRING(DAY(oe-rel.rel-date),"99")
                                   ELSE IF cSort EQ "Item Name" THEN oe-ordl.i-name
                                   ELSE ""
                    tt-report.key-02  = IF cSort EQ "Item No"   THEN oe-rel.i-no
                                   ELSE IF cSort EQ "Territory" THEN cust.terr
                                   ELSE IF cSort EQ "Carrier"   THEN oe-rel.carrier
                                   ELSE IF cSort EQ "Credit"    THEN cust.cr-rating
                                   ELSE oe-rel.cust-no
                    tt-report.key-03  = IF cSort NE "Release Date" THEN
                                            STRING(YEAR(oe-rel.rel-date),"9999") +
                                            STRING(MONTH(oe-rel.rel-date),"99")  +
                                            STRING(DAY(oe-rel.rel-date),"99")
                                        ELSE ""
                    tt-report.key-04  = STRING(IF cSort EQ "Carrier" THEN oe-rel.cust-no ELSE " ","x(10)") +
                                        STRING(oe-ord.ord-no,"9999999999")
                    tt-report.key-05  = STRING(INDEX(cTypes,cType),"99")
                    tt-report.key-06  = cType
                    tt-report.key-07  = "No"
                    tt-report.rec-id  = RECID(oe-rel)
                    iTotal            = iTotal + 1
                    .
                IF lProgressBar THEN
                RUN spProgressBar (cProgressBar, iTotal, ?).
            END. /* if index ctypes */
        END. /* each oe-rel */
        
        FOR EACH oe-rell NO-LOCK
            WHERE oe-rell.company EQ oe-ordl.company
              AND oe-rell.ord-no  EQ oe-ordl.ord-no
              AND oe-rell.i-no    EQ oe-ordl.i-no
              AND oe-rell.line    EQ oe-ordl.line
              AND ((oe-rell.b-ord-no NE 0 AND INDEX(cTypes,"B") GT 0)
               OR  (oe-rell.b-ord-no EQ 0 AND INDEX(cTypes,"A") GT 0))
              AND oe-rell.loc GE cStartShipFromNo
              AND oe-rell.loc LE cEndShipFromNo
            USE-INDEX ord-no,
            FIRST oe-relh NO-LOCK
            WHERE oe-relh.r-no     EQ oe-rell.r-no
              AND oe-relh.posted   EQ NO
              AND oe-relh.deleted  EQ NO
              AND oe-relh.rel-date GE dtStartReleaseDate
              AND oe-relh.rel-date LE dtEndReleaseDate
              AND oe-relh.carrier  GE cStartCarrNo
              AND oe-relh.carrier  LE cEndCarrNo
            USE-INDEX r-no
            BREAK BY oe-rell.r-no
                  BY oe-rell.ord-no
                  BY oe-rell.i-no
                  BY oe-rell.line
                  BY oe-rell.rel-no
                  BY oe-rell.b-ord-no
                  BY oe-rell.po-no
            :
            IF FIRST-OF(oe-rell.po-no) THEN iQty = 0.
            iQty = iQty + oe-rell.qty.  
            IF LAST-OF(oe-rell.po-no) THEN DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = IF cSort EQ "Release Date" THEN
                                            STRING(YEAR(oe-relh.rel-date),"9999") +
                                            STRING(MONTH(oe-relh.rel-date),"99")  +
                                            STRING(DAY(oe-relh.rel-date),"99")
                                   ELSE IF cSort EQ "Item Name" THEN oe-ordl.i-name
                                   ELSE ""
                    tt-report.key-02  = IF cSort EQ "Item No"   THEN oe-rell.i-no
                                   ELSE IF cSort EQ "Territory" THEN cust.terr
                                   ELSE IF cSort EQ "Carrier"   THEN oe-relh.carrier
                                   ELSE IF cSort EQ "Credit"    THEN cust.cr-rating
                                   ELSE oe-relh.cust-no
                    tt-report.key-03  = IF cSort NE "Release Date" THEN
                                            STRING(YEAR(oe-relh.rel-date),"9999") +
                                            STRING(MONTH(oe-relh.rel-date),"99")  +
                                            STRING(DAY(oe-relh.rel-date),"99")
                                        ELSE ""
                    tt-report.key-04  = STRING(IF cSort EQ "Carrier" THEN oe-relh.cust-no ELSE " ","x(10)") +
                                        STRING(oe-ord.ord-no,"9999999999")
                    tt-report.key-05  = STRING(INDEX(cTypes,cType),"99")
                    tt-report.key-06  = IF oe-rell.b-ord-no EQ 0 THEN "A" ELSE "B"
                    tt-report.key-07  = STRING(oe-relh.printed)
                    tt-report.qty     = iQty
                    tt-report.rec-id  = RECID(oe-rell)
                    iTotal            = iTotal + 1
                    .
                IF lProgressBar THEN
                RUN spProgressBar (cProgressBar, iTotal, ?).
            END. /* if last-of */
        END. /* each oe-rell */
    END. /* each oe-ordl */
    
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND (tt-report.key-07 EQ cPrinted OR cPrinted EQ "All")
        BREAK BY tt-report.key-01
              BY tt-report.key-02
              BY tt-report.key-03
              BY tt-report.key-04
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, iTotal).
        RELEASE oe-rel.
        RELEASE oe-rell.
        RELEASE oe-relh.
        RELEASE oe-ord.
        RELEASE oe-ordl.
        FIND FIRST oe-rel NO-LOCK  
             WHERE RECID(oe-rel) EQ tt-report.rec-id
             NO-ERROR.
        ASSIGN
            iReleaseNo = 0
            cShipFrom  = ""
            .
        IF AVAILABLE oe-rel THEN DO:
            cShipFrom = oe-rel.spare-char-1.
            FOR EACH oe-rell NO-LOCK
                WHERE oe-rell.company  EQ oe-rel.company
                  AND oe-rell.ord-no   EQ oe-rel.ord-no
                  AND oe-rell.rel-no   EQ oe-rel.rel-no
                  AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                  AND oe-rell.i-no     EQ oe-rel.i-no
                  AND oe-rell.line     EQ oe-rel.line
                USE-INDEX ord-no,
                FIRST oe-relh NO-LOCK
                WHERE oe-relh.r-no EQ oe-rell.r-no
                :
                ASSIGN 
                    iReleaseNo = oe-relh.release#
                    cShipFrom  = oe-rell.loc
                    .
                IF oe-relh.posted EQ NO AND oe-relh.deleted EQ NO THEN
                tt-report.rec-id = RECID(oe-rell).
                ELSE RELEASE oe-relh.
                LEAVE.
            END. /* each oe-rell */       
            FIND FIRST oe-ordl NO-LOCK 
                 WHERE oe-ordl.company EQ oe-rel.company
                   AND oe-ordl.ord-no  EQ oe-rel.ord-no
                   AND oe-ordl.i-no    EQ oe-rel.i-no
                   AND oe-ordl.line    EQ oe-rel.line
                NO-ERROR.
        END. /* avail oe-rel */    
        IF NOT AVAILABLE oe-rell THEN
        FIND oe-rell NO-LOCK
             WHERE RECID(oe-rell) EQ tt-report.rec-id
             NO-ERROR.
        IF AVAILABLE oe-rell THEN DO:    
            IF INDEX("SLI",tt-report.key-06) GT 0 THEN
            tt-report.key-06 = IF oe-rell.b-ord-no EQ 0 THEN "A" ELSE "B".
            FIND FIRST oe-relh NO-LOCK 
                 WHERE oe-relh.company EQ oe-rell.company
                   AND oe-relh.r-no    EQ oe-rell.r-no
                 USE-INDEX r-no.
            ASSIGN 
                iReleaseNo = IF AVAILABLE oe-relh THEN oe-relh.release# ELSE iReleaseNo
                cShipFrom  = oe-rell.loc
                .
            FIND FIRST oe-ordl NO-LOCK 
                WHERE oe-ordl.company EQ oe-rell.company
                  AND oe-ordl.ord-no  EQ oe-rell.ord-no
                  AND oe-ordl.i-no    EQ oe-rell.i-no
                  AND oe-ordl.line    EQ oe-rell.line
                NO-ERROR.
        END. /* avail oe-rell */
        FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.    
        IF AVAILABLE oe-ord THEN
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ oe-ord.company
               AND cust.cust-no EQ oe-ord.cust-no
             NO-ERROR.
        IF AVAILABLE oe-relh THEN DO:
            iQty = 0 .
            FOR EACH  bf-oe-rell NO-LOCK
                WHERE bf-oe-rell.company  EQ oe-rell.company
                  AND bf-oe-rell.ord-no   EQ oe-rell.ord-no
                  AND bf-oe-rell.rel-no   EQ oe-rell.rel-no
                  AND bf-oe-rell.b-ord-no EQ oe-rell.b-ord-no
                  AND bf-oe-rell.i-no     EQ oe-rell.i-no 
                  AND bf-oe-rell.line     EQ oe-rell.line
                USE-INDEX ord-no
                :
                iQty  = iQty + bf-oe-rell.qty .
            END. /* each bf-oe-rell */
            ASSIGN
                dtDate   = oe-relh.rel-date 
                cPONum   = oe-rell.po-no
                cShipID  = oe-relh.ship-id
                cCarrier = oe-relh.carrier
                cReltyp  = oe-rell.s-code
                lPrinted = oe-relh.printed
                .      
        END. /* avail bf-oe-rell */
        ELSE IF AVAILABLE oe-rel THEN
             ASSIGN
                 iQty     = IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty 
                 dtDate   = oe-rel.rel-date
                 cPONum   = oe-rel.po-no
                 cShipID  = oe-rel.ship-id
                 cCarrier = oe-rel.carrier
                 cReltyp  = oe-rel.s-code
                 lPrinted = NO
                 .
        cRelDueDate = IF AVAIL oe-rel THEN STRING(ENTRY(1, oe-rel.spare-char-4)) ELSE "".
        CREATE w-ord.
        IF AVAILABLE oe-ordl THEN DO:
            FIND FIRST itemfg NO-LOCK 
                 WHERE itemfg.company EQ oe-ordl.company
                   AND itemfg.i-no    EQ oe-ordl.i-no
                 NO-ERROR.
            ASSIGN
                w-ord.ord-no         = oe-ord.ord-no
                w-ord.cust-no        = oe-ord.cust-no
                w-ord.cust-name      = oe-ord.cust-name
                w-ord.part-no        = oe-ordl.part-no
                w-ord.i-no           = oe-ordl.i-no
                w-ord.i-name         = oe-ordl.i-name
                w-ord.qty            = oe-ordl.qty
                w-ord.cost           = oe-ordl.cost
                w-ord.price          = oe-ordl.t-price / oe-ordl.qty
                w-ord.rel-qty        = iQty
                w-ord.t-price        = w-ord.price * w-ord.rel-qty
                w-ord.rel-date       = STRING(dtDate) + tt-report.key-06
                w-ord.xls-rel-date   = dtDate
                w-ord.xls-status     = tt-report.key-06
                w-ord.rel-no         = iReleaseNo
                w-ord.ship-id        = cShipID
                w-ord.job-no         = oe-ordl.job-no
                w-ord.job-no2        = oe-ordl.job-no2
                w-ord.job            = IF w-ord.job-no EQ "" THEN "" ELSE
                                      (TRIM(w-ord.job-no) + "-" + STRING(w-ord.job-no2,"99"))
                w-ord.po-num         = cPONum
                w-ord.ord-qty        = oe-ordl.qty
                w-ord.due-date       = oe-ordl.req-date
                w-ord.shp-qty        = oe-ordl.ship-qty
                w-ord.msf            = w-ord.rel-qty * itemfg.t-sqft / 1000
                w-ord.prom-code      = oe-ordl.prom-code
                w-ord.last-date      = oe-ord.last-date
                w-ord.carrier        = cCarrier 
                w-ord.ship-from      = cShipFrom
                w-ord.sman           = oe-ordl.s-man[1] 
                w-ord.upd-user       = oe-ord.USER-ID
                w-ord.is-a-component = oe-ordl.is-a-component
                dPallets             = w-ord.rel-qty / 
                                     ((IF oe-ordl.cas-cnt    EQ 0 THEN 1 ELSE oe-ordl.cas-cnt) *
                                      (IF oe-ordl.cases-unit EQ 0 THEN 1 ELSE oe-ordl.cases-unit))
                w-ord.csrUser_id     = oe-ord.csrUser_id
                w-ord.cType          = cReltyp
                w-ord.entered-id     = oe-ord.entered-id 
                w-ord.ord-due-date   = IF oe-ord.due-date NE ? THEN STRING(oe-ord.due-date) ELSE ""  
                w-ord.rel-due-date   = IF cRelDueDate NE ? THEN cRelDueDate ELSE "" 
                w-ord.Printed        = lPrinted 
                w-ord.promiseDate    = IF oe-ord.promiseDate NE ? THEN STRING(oe-ord.promiseDate) ELSE ""
                w-ord.priority       = oe-ord.priority
                w-ord.pr-uom         = oe-ordl.pr-uom
                .
            {sys/inc/roundup.i dPallets}
            IF dPallets LT 0 THEN dPallets = dPallets * -1.
            w-ord.palls = w-ord.palls + dPallets.
            IF NOT FIRST-OF(tt-report.key-02) AND cSort EQ "Customer No" THEN w-ord.cust-name = "".
            IF lGetVendorPOInfo THEN
            RUN pGetVendorPOInfo (
                oe-ordl.po-no-po,
                oe-ordl.job-no,
                oe-ordl.job-no2,
                OUTPUT w-ord.vend-id,
                OUTPUT w-ord.vend-name,
                OUTPUT w-ord.vend-po,
                OUTPUT w-ord.po-due-date,
                OUTPUT w-ord.po-rm-item,
                OUTPUT w-ord.po-rm-item-name,
                OUTPUT w-ord.po-uom,
                OUTPUT w-ord.po-ord-qty,
                OUTPUT w-ord.po-rec-qty
                ).
        END. /* avail oe-ordl */
    
        FOR EACH w-ord
            BREAK BY w-ord.component
                  BY w-ord.i-no
            :
            FIND FIRST itemfg NO-LOCK 
                 WHERE itemfg.company EQ cCompany
                   AND itemfg.i-no    EQ w-ord.i-no
                 NO-ERROR.
            IF CAN-FIND(FIRST dynValueColumn
                        WHERE dynValueColumn.subjectID EQ dynValueParam.subjectID
                          AND dynValueColumn.user-id   EQ dynValueParam.user-id
                          AND dynValueColumn.prgmName  EQ dynValueParam.prgmName
                          AND dynValueColumn.paramValueID EQ dynValueParam.paramValueID) AND
               w-ord.component EQ 0 AND w-ord.job-no NE "" THEN
            FOR EACH fg-bin FIELDS(qty) NO-LOCK
                WHERE fg-bin.company EQ cCompany
                  AND fg-bin.i-no    EQ w-ord.i-no
                  AND fg-bin.job-no  EQ w-ord.job-no
                  AND fg-bin.job-no2 EQ w-ord.job-no2
                  AND fg-bin.loc     GE cStartLoc
                  AND fg-bin.loc     LE cEndLoc
                USE-INDEX job
                :
                w-ord.onh-qty = w-ord.onh-qty + fg-bin.qty.
            END. /* each fg-bin */
            FOR EACH fg-bin FIELDS(qty) NO-LOCK
                WHERE fg-bin.company EQ cCompany
                  AND fg-bin.i-no    EQ w-ord.i-no
                  AND fg-bin.loc     GE cStartLoc
                  AND fg-bin.loc     LE cEndLoc
                USE-INDEX i-no
                :
                w-ord.tot-qty = w-ord.tot-qty + fg-bin.qty.
            END. /* each fg-bin */
            iOHRelQty = w-ord.tot-qty - w-ord.rel-qty.
            
            IF lOnlyNegativeAvailable AND AVAILABLE itemfg AND itemfg.q-avail GE 0 THEN NEXT.
            IF lOnlyNegOHRelQty AND iOHRelQty GE 0 THEN NEXT.
    
            BUFFER bw-ord:FIND-BY-ROWID(ROWID(w-ord), NO-LOCK) .
            FIND FIRST cust NO-LOCK
                 WHERE cust.company EQ cCompany
                   AND cust.cust-no EQ w-ord.cust-no
                 NO-ERROR.
            IF AVAILABLE cust THEN
            ASSIGN
                cCRRate    = cust.cr-rating
                cTerritory = cust.terr
                cDelZone   = cust.del-zone
                .
            ELSE
            ASSIGN
                cCRRate    = ""
                cTerritory = ""
                cDelZone   = ""
                .
            FIND FIRST shipto NO-LOCK
                 WHERE shipto.company = cCompany
                   AND shipto.cust-no = w-ord.cust-no
                   AND shipto.ship-id = w-ord.ship-id
                 NO-ERROR.
            IF AVAILABLE shipto THEN 
            ASSIGN 
                cDelZone   = shipto.dest-code
                cShipAddr1 = shipto.ship-addr[1]
                cShipAddr2 = shipto.ship-addr[2]
                cShipCity  = shipto.ship-city
                cShipState = shipto.ship-state
                cShipZip   = shipto.ship-zip
                cShipName  = shipto.ship-name
                cDockHr    = shipto.dock-hour
                .
            ELSE 
            ASSIGN
                cDelZone   = ""
                cShipAddr1 = ""
                cShipAddr2 = ""
                cShipCity  = ""
                cShipState = ""
                cShipZip   = ""
                cShipName  = ""
                cDockHr    = ""
                .
            IF cPrintOHQty EQ "Qty OH = 0"         AND  w-ord.tot-qty NE 0 THEN NEXT.
            IF cPrintOHQty EQ "Qty OH < Order Qty" AND (w-ord.tot-qty GT w-ord.onh-qty OR w-ord.onh-qty EQ 0) THEN NEXT.
            IF cPrintOHQty EQ "Qty OH > Order Qty" AND (w-ord.tot-qty LT w-ord.onh-qty OR w-ord.tot-qty EQ 0) THEN NEXT.
            ASSIGN
                cReasonCode = ""
                cReasonDesc = ""
                .
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company EQ cCompany
                   AND job-hdr.job-no  EQ w-ord.job-no
                   AND job-hdr.job-no2 EQ w-ord.job-no2
                   AND job-hdr.ord-no  EQ w-ord.ord-no
                   AND job-hdr.i-no    EQ w-ord.i-no
                 NO-ERROR.
            IF NOT AVAILABLE job-hdr THEN
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company EQ cCompany
                   AND job-hdr.job-no  EQ w-ord.job-no
                   AND job-hdr.job-no2 EQ w-ord.job-no2
                   AND job-hdr.ord-no  EQ w-ord.ord-no
                 NO-ERROR.
            IF AVAILABLE job-hdr THEN
            FIND FIRST job NO-LOCK
                 WHERE job.company EQ job-hdr.company
                   AND job.job     EQ job-hdr.job
                   AND job.job-no  EQ job-hdr.job-no
                   AND job.job-no2 EQ job-hdr.job-no2
                 NO-ERROR.
            IF AVAILABLE job AND job.stat = "H" THEN DO: 
                FIND FIRST rejct-cd NO-LOCK
                     WHERE rejct-cd.type EQ "JH" 
                       AND rejct-cd.code EQ job.reason
                     NO-ERROR.
                IF AVAILABLE job-hdr  AND
                   AVAILABLE rejct-cd AND
                   AVAILABLE job      AND job.job-no NE "" THEN
                ASSIGN
                    cReasonCode = job.reason
                    cReasonDesc = rejct-cd.dscr
                    .      
            END. /* avail job-hdr */
            IF lSubRpt_PrintSpecNotes AND AVAILABLE itemfg THEN 
            RUN pPrintSpecNotes (itemfg.rec_key).
            RUN jobRouting (OUTPUT lRunComplete, OUTPUT cRouting).    
            
            FOR EACH fg-rcpth
                    WHERE fg-rcpth.company = cCompany 
                    AND fg-rcpth.i-no    = w-ord.i-no
                    AND fg-rcpth.rita-code  = "R" 
                    AND fg-rcpth.job-no  = w-ord.job-no
                    AND fg-rcpth.job-no2 = w-ord.job-no2
                    NO-LOCK
                    BREAK BY fg-rcpth.trans-date DESCENDING:
                w-ord.dLastRecDate = fg-rcpth.trans-date. 
                LEAVE.
            END.    
    
            CREATE ttScheduledReleases.
            ASSIGN
                ttScheduledReleases.jobNo           = IF w-ord.job-no EQ "" THEN ""
                                                      ELSE w-ord.job-no + "-" + STRING(w-ord.job-no2,"99")
                ttScheduledReleases.customerName    = w-ord.cust-name
                ttScheduledReleases.shipTo          = w-ord.ship-id
                ttScheduledReleases.poNo            = w-ord.po-num
                ttScheduledReleases.orderNo         = w-ord.ord-no
                ttScheduledReleases.relNo           = w-ord.rel-no
                ttScheduledReleases.itemNo          = w-ord.i-no
                ttScheduledReleases.itemDescription = w-ord.i-name
                ttScheduledReleases.relQty          = w-ord.rel-qty
                ttScheduledReleases.relDate         = DATE(w-ord.rel-date)
                ttScheduledReleases.relStat         = w-ord.xls-status
                ttScheduledReleases.dueAlert        = w-ord.prom-code
                ttScheduledReleases.carrier         = w-ord.carrier
                ttScheduledReleases.jobQtyOH        = w-ord.onh-qty
                ttScheduledReleases.totQtyOH        = w-ord.tot-qty
                ttScheduledReleases.salesValue      = w-ord.t-price
                ttScheduledReleases.orderQty        = w-ord.ord-qty
                ttScheduledReleases.msf             = w-ord.msf
                ttScheduledReleases.shippedQty      = w-ord.shp-qty
                ttScheduledReleases.custNo          = w-ord.cust-no
                ttScheduledReleases.customerPartNo  = w-ord.part-no
                ttScheduledReleases.delZone         = cDelZone
                ttScheduledReleases.terr            = cTerritory
                ttScheduledReleases.creditRating    = cCRRate
                ttScheduledReleases.routing         = cRouting
                ttScheduledReleases.skidQty         = w-ord.palls
                ttScheduledReleases.ohRelQty        = iOHRelQty
                ttScheduledReleases.sampleDate      = IF AVAILABLE shipto THEN w-ord.xls-rel-date - INT(shipto.spare-int-1) ELSE ?
                ttScheduledReleases.dockDate        = IF AVAILABLE shipto THEN w-ord.xls-rel-date - INT(shipto.spare-int-2) ELSE ?
                ttScheduledReleases.earlyDate       = IF AVAILABLE shipto THEN w-ord.xls-rel-date - INT(shipto.spare-int-3) ELSE ?
                ttScheduledReleases.lateDate        = IF AVAILABLE shipto THEN w-ord.xls-rel-date - INT(shipto.spare-int-4) ELSE ?
                ttScheduledReleases.transitDays     = IF AVAILABLE shipto THEN INT(shipto.del-time) ELSE 0
                ttScheduledReleases.state           = IF AVAILABLE shipto THEN shipto.ship-state ELSE ""
                ttScheduledReleases.totalAlloc      = IF AVAILABLE itemfg THEN itemfg.q-alloc ELSE 0
                ttScheduledReleases.totalAvail      = IF AVAILABLE itemfg THEN itemfg.q-avail ELSE 0
                ttScheduledReleases.shipFrom        = w-ord.ship-from
                ttScheduledReleases.dockNote        = IF AVAILABLE shipto THEN shipto.dock-hour ELSE ""
                ttScheduledReleases.salRep          = w-ord.sman
                ttScheduledReleases.lastUserID      = w-ord.upd-user
                ttScheduledReleases.shipToAdd1      = cShipAddr1
                ttScheduledReleases.shipToAdd2      = cShipAddr2
                ttScheduledReleases.shipToCity      = cShipCity
                ttScheduledReleases.shipToState     = cShipState
                ttScheduledReleases.shipToZip       = cShipZip
                ttScheduledReleases.shipToName      = cShipName
                ttScheduledReleases.dueDate         = w-ord.due-date
                ttScheduledReleases.style           = IF AVAILABLE itemfg THEN itemfg.style ELSE ""
                ttScheduledReleases.runComplete     = lRunComplete
                ttScheduledReleases.fgCategory      = IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
                ttScheduledReleases.overRunPct      = IF AVAILABLE cust THEN cust.over-pct ELSE 0.0
                ttScheduledReleases.jobHoldCode     = cReasonCode
                ttScheduledReleases.jobHoldDesc     = cReasonDesc
                ttScheduledReleases.xxItemFGRecKey  = IF AVAILABLE itemfg THEN itemfg.rec_key ELSE ""
                ttScheduledReleases.xxSort01        = tt-report.key-01
                ttScheduledReleases.xxSort02        = tt-report.key-02
                ttScheduledReleases.xxSort03        = tt-report.key-03
                ttScheduledReleases.xxSort04        = tt-report.key-04
                ttScheduledReleases.csrUserID       = w-ord.csrUser_id
                ttScheduledReleases.cDockHour       = cDockHr
                ttScheduledReleases.cRelType        = w-ord.cType
                ttScheduledReleases.dLastDate       = w-ord.dLastRecDate
                ttScheduledReleases.enteredBy       = w-ord.entered-id 
                ttScheduledReleases.orderDueDate    = w-ord.ord-due-date
                ttScheduledReleases.releaseDueDate  = w-ord.rel-due-date
                ttScheduledReleases.printed         = w-ord.Printed
                ttScheduledReleases.orderPromDate   = w-ord.promiseDate
                ttScheduledReleases.orderPriority   = w-ord.priority
                ttScheduledReleases.vendorID        = w-ord.vend-id
                ttScheduledReleases.vendorName      = w-ord.vend-name
                ttScheduledReleases.vendorPO        = w-ord.vend-po 
                ttScheduledReleases.poDueDate       = w-ord.po-due-date
                ttScheduledReleases.poRMItemNo      = w-ord.po-rm-item
                ttScheduledReleases.poRMItemName    = w-ord.po-rm-item-name
                ttScheduledReleases.poOUM           = w-ord.po-uom
                ttScheduledReleases.poOrderQty      = w-ord.po-ord-qty
                ttScheduledReleases.poReceivedQty   = w-ord.po-rec-qty
                ttScheduledReleases.lastShipDate    = STRING(w-ord.last-date,"99/99/9999")
                ttScheduledReleases.priceUOM        = w-ord.pr-uom
                ttScheduledReleases.price           = w-ord.price
                .
        END. /* each w-ord */
        EMPTY TEMP-TABLE w-ord.
    END. /* each tt-report */
END PROCEDURE.

PROCEDURE jobRouting:
    DEFINE OUTPUT PARAMETER oplRunComplete AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRouting     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lRunComplete AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRouting     AS CHARACTER NO-UNDO.
    
    FOR EACH tt-fg-set:
        DELETE tt-fg-set.
    END.
    
    RELEASE job-hdr.
    RELEASE job.
    RELEASE reftable.
    
    IF TRIM(w-ord.job-no) EQ "" THEN DO:
        IF AVAIL itemfg AND itemfg.est-no NE "" THEN
        FOR EACH est-op NO-LOCK
            WHERE est-op.company EQ itemfg.company 
              AND est-op.est-no  EQ itemfg.est-no 
              AND est-op.line    LT 500
            BREAK BY est-op.line
            :
            IF FIRST(est-op.line) AND cRouting NE "" THEN 
            cRouting = cRouting + ",".
            cRouting = cRouting + est-op.m-code + ",".
            IF NOT LAST(est-op.line) THEN 
            cRouting = TRIM(cRouting,",").
        END. /* each est-op */
    END. /* if trim */
    ELSE DO:
        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ cCompany
              AND job-hdr.job-no  EQ w-ord.job-no
              AND job-hdr.job-no2 EQ w-ord.job-no2
              AND job-hdr.ord-no  EQ w-ord.ord-no
              AND job-hdr.i-no    EQ w-ord.i-no
            NO-ERROR.
        IF NOT AVAILABLE job-hdr THEN
            FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ cCompany
                  AND job-hdr.job-no  EQ w-ord.job-no
                  AND job-hdr.job-no2 EQ w-ord.job-no2
                  AND job-hdr.ord-no  EQ w-ord.ord-no
                NO-ERROR.
    END. /* else */
    
    IF AVAILABLE job-hdr THEN
        FIND FIRST job NO-LOCK 
            WHERE job.company EQ job-hdr.company
              AND job.job     EQ job-hdr.job
              AND job.job-no  EQ job-hdr.job-no
              AND job.job-no2 EQ job-hdr.job-no2
            NO-ERROR.
    IF AVAILABLE job /*AND AVAILABLE itemfg*/ THEN DO:
        IF (itemfg.isaset OR w-ord.is-a-component) AND
            CAN-FIND(FIRST reftable
                     WHERE reftable.reftable EQ "jc/jc-calc.p"
                       AND reftable.company  EQ job.company
                       AND reftable.loc      EQ ""
                       AND reftable.code     EQ STRING(job.job,"999999999")) THEN
            FOR EACH reftable NO-LOCK 
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                  AND reftable.company  EQ job-hdr.company
                  AND reftable.loc      EQ ""
                  AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                  AND ((reftable.code2  EQ w-ord.i-no AND w-ord.is-a-component) OR
                       (job-hdr.i-no    EQ w-ord.i-no AND NOT w-ord.is-a-component))
                :
                CREATE tt-fg-set.
                ASSIGN
                    tt-fg-set.part-no      = reftable.code2
                    tt-fg-set.qtyPerSet    = reftable.val[12]
                    tt-fg-set.part-qty-dec = reftable.val[13]
                    .
            END. /* each reftable */
        ELSE DO:
            CREATE tt-fg-set.
            ASSIGN
                tt-fg-set.part-no      = job-hdr.i-no
                tt-fg-set.qtyPerSet    = job-hdr.frm
                tt-fg-set.part-qty-dec = job-hdr.blank-no
                .
        END. /* else */
        RELEASE tt-fg-set.
    
        FOR EACH tt-fg-set
            BREAK BY tt-fg-set.qtyPerSet
                  BY tt-fg-set.part-qty-dec
            :
            FOR EACH job-mch NO-LOCK 
                WHERE job-mch.company EQ job.company
                  AND job-mch.job     EQ job.job
                  AND job-mch.job-no  EQ job.job-no
                  AND job-mch.job-no2 EQ job.job-no2
                  AND job-mch.frm     EQ integer(tt-fg-set.qtyPerSet)
                BREAK BY job-mch.line
                :
                cRouting = cRouting + job-mch.m-code + ",".
                IF LAST(job-mch.line) THEN
                ASSIGN 
                    cRouting = TRIM(cRouting,",")
                    lRunComplete = job-mch.run-complete
                    .
            END. /* each job-mch */
            IF lSubRpt_PrintScheduleStats THEN DO:
                CREATE ttScheduledReleasesStats.
                ASSIGN 
                    ttScheduledReleasesStats.xxItemFGRecKey = itemfg.rec_key
                    ttScheduledReleasesStats.statsLine      = "S/B: " + TRIM(STRING(tt-fg-set.qtyPerSet,"->>,>>9.99<<<<"))
                                                            + "/"     + TRIM(STRING(tt-fg-set.part-qty-dec,">>"))
                    .
                IF LAST-OF(tt-fg-set.qtyPerSet) THEN DO:
                    RUN pPrintScheduleStats.
                    IF cRouting NE "" THEN 
                    ttScheduledReleasesStats.statsLine = ttScheduledReleasesStats.statsLine
                                                       + " Routing: " + cRouting.
                END. /* last-of */
            END. /* if print stats */
        END. /* each tt-fg-set */
    END. /* avail job */
    ASSIGN 
        oplRunComplete = lRunComplete
        opcRouting     = cRouting
        .
END PROCEDURE.

PROCEDURE pPrintScheduleStats:
    DEFINE VARIABLE iQtyOrd AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQtyRec AS INTEGER NO-UNDO.

    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company   EQ job.company
          AND po-ordl.job-no    EQ job.job-no
          AND po-ordl.job-no2   EQ job.job-no2
          AND po-ordl.s-num     EQ tt-fg-set.qtyPerSet
          AND po-ordl.item-type EQ YES
        USE-INDEX job-no,
        FIRST po-ord NO-LOCK 
        WHERE po-ord.company EQ po-ordl.company
          AND po-ord.po-no   EQ po-ordl.po-no,
        FIRST item NO-LOCK 
        WHERE item.company EQ po-ordl.company
          AND item.i-no    EQ po-ordl.i-no
          AND INDEX("1234BPR",item.mat-type) GT 0
        BREAK BY po-ordl.po-no
              BY po-ordl.i-no
              BY po-ordl.rec_key
        :
        IF po-ordl.pr-qty-uom EQ "EA" THEN
            iQtyOrd = po-ordl.ord-qty.
        ELSE
            RUN sys/ref/convquom.p
               (po-ordl.pr-qty-uom, "EA",
                item.basis-w,
                po-ordl.s-len,
                po-ordl.s-wid,
                item.s-dep,
                po-ordl.ord-qty,
                OUTPUT iQtyOrd
               ).
        {sys/inc/roundup.i iQtyOrd}
        IF po-ordl.cons-uom EQ "EA" THEN
            iQtyRec = po-ordl.t-rec-qty.
        ELSE
            RUN sys/ref/convquom.p
               (po-ordl.cons-uom, "EA",
                item.basis-w,
                po-ordl.s-len,
                po-ordl.s-wid,
                item.s-dep,
                po-ordl.t-rec-qty,
                OUTPUT iQtyRec
               ).
        {sys/inc/roundup.i iQtyRec}        
        ttScheduledReleasesStats.statsLine = ttScheduledReleasesStats.statsLine
                                           + " Brd PO#: "   + TRIM(STRING(po-ordl.po-no,">>>>>>>>"))
                                           + " Vendor: "    + po-ord.vend-no
                                           + " Qty Rec'd: " + TRIM(STRING(iQtyRec,">>>,>>>,>>9"))
                                           .
    END. /* each po-ordl */    
END PROCEDURE.

PROCEDURE pGetVendorPOInfo :
    DEFINE INPUT  PARAMETER ipiPoNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendor     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendorName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiVendorPo   AS INTEGER   NO-UNDO.   
    DEFINE OUTPUT PARAMETER opdtPoDueDate AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRMItem     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRMItemName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUom        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOrdQty     AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRecQty     AS DECIMAL   NO-UNDO.
  
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    FIND FIRST po-ordl NO-LOCK USE-INDEX po-no
         WHERE po-ordl.company   EQ cCompany
           AND po-ordl.po-no     EQ ipiPoNo
           AND po-ordl.job-no    EQ ipcJobNo
           AND po-ordl.job-no2   EQ ipiJobNo2
           AND po-ordl.item-type EQ YES
         NO-ERROR.
    IF AVAILABLE po-ordl THEN
    DO:
        FIND FIRST po-ord NO-LOCK
             WHERE po-ord.company EQ po-ordl.company
               AND po-ord.po-no   EQ po-ordl.po-no
             NO-ERROR.
        IF AVAILABLE po-ord THEN
        DO:
            ASSIGN
                opcVendor     = po-ord.vend-no           
                opiVendorPo   = po-ord.po-no
                opdtPoDueDate = po-ord.due-date
                opcRMItem     = po-ordl.i-no 
                opcRMItemName = po-ordl.i-name
                opcUom        = "EA"
                .           
            FIND FIRST item NO-LOCK
                 WHERE item.company EQ job.company
                   AND item.i-no    EQ po-ordl.i-no
                 NO-ERROR.                
            ASSIGN
                opdOrdQty = po-ordl.cons-qty
                opdRecQty = po-ordl.t-rec-qty
                .           
            IF po-ordl.cons-uom NE opcUom THEN 
            DO:                      
                IF po-ordl.cons-qty NE 0 THEN               
                    RUN Conv_QuantityFromUOMtoUOM (
                        cCompany,
                        po-ordl.i-no,
                        "RM",
                        po-ordl.cons-qty,
                        po-ordl.cons-uom,
                        opcUom,
                        IF AVAILABLE item THEN item.basis-w ELSE 0,
                        po-ordl.s-len,
                        po-ordl.s-wid,
                        IF AVAILABLE item THEN item.s-dep ELSE 0,
                        0,
                        OUTPUT opdOrdQty,
                        OUTPUT lError,
                        OUTPUT cMessage
                        ).     
                IF po-ordl.t-rec-qty NE 0 THEN             
                    RUN Conv_QuantityFromUOMtoUOM (
                        cCompany,
                        po-ordl.i-no,
                        "RM",
                        po-ordl.t-rec-qty,
                        po-ordl.cons-uom,
                        opcUom,
                        IF AVAILABLE item THEN item.basis-w ELSE 0,
                        po-ordl.s-len,
                        po-ordl.s-wid,
                        IF AVAILABLE item THEN item.s-dep ELSE 0,
                        0,
                        OUTPUT opdRecQty,
                        OUTPUT lError,
                        OUTPUT cMessage
                        ).                  
            END.
            FIND FIRST vend NO-LOCK
                 WHERE vend.company EQ cCompany
                   AND vend.vend-no EQ po-ord.vend-no
                 NO-ERROR.
            IF AVAILABLE vend THEN     
            opcVendorName = vend.name.
        END. 
    END.  
END PROCEDURE.

PROCEDURE pPrintSpecNotes:
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
    
    FOR EACH notes NO-LOCK
        WHERE notes.rec_key   EQ ipcRecKey
          AND notes.note_type EQ "S"
          AND notes.note_code GE cStartSpecNote
          AND notes.note_code LE cEndSpecNote      
        BREAK BY notes.note_code
        :
        IF FIRST-OF(notes.note_code) THEN DO: 
            CREATE ttScheduledReleasesNotes.
            ASSIGN 
                ttScheduledReleasesNotes.xxItemFGRecKey = ipcRecKey
                ttScheduledReleasesNotes.noteCode       = notes.note_code
                .
        END. /* first-of */
        ttScheduledReleasesNotes.noteText = ttScheduledReleasesNotes.noteText + " " + TRIM(notes.note_text).     
    END. /* each notes */
END PROCEDURE.

{AOA/BL/pGetCustInfo.i}
