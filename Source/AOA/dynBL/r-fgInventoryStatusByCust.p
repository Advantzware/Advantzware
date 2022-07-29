/*------------------------------------------------------------------------
  File:         FGInventoryStatus.p
  Description:  Business Logic
  Author:       Sachin Chahal
  Date Created: 05.31.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttFGInventoryStatus
{aoa/tempTable/ttFGInventoryStatus.i}

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE tt-cust
    FIELD cust-no  AS CHARACTER  
    FIELD cust-row AS ROWID
    FIELD slsrep   AS CHARACTER
    INDEX i1 cust-no 
    INDEX i2 cust-row.

/* Local Variable Definitions ---                                       */

&Scoped-define subjectID 205
{AOA/includes/subjectID{&subjectID}Defs.i}

/* subject business logic */
/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE iCount         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSlsRep        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iQtyOrd        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQtyShip       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dExt           AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE dQtyOnhand     AS DECIMAL   FORMAT "->>>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE dtTransDate    LIKE fg-rcpts.trans-date NO-UNDO.
    DEFINE VARIABLE cJobNo         AS CHARACTER FORMAT "x(13)" NO-UNDO.
    DEFINE VARIABLE dQtyJob        LIKE dQtyOnhand NO-UNDO.
    DEFINE VARIABLE dExtJob        LIKE dExt NO-UNDO.
    DEFINE VARIABLE dSellprice     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCommited      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cLotNum        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustLotNum    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrderNo       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSalesRep      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustOwn       AS LOGICAL   FORMAT "Y/N" INIT "N" NO-UNDO.
    DEFINE VARIABLE lZeroQtyOnHand AS LOGICAL   FORMAT "Y/N" INIT "N" NO-UNDO.
    DEFINE VARIABLE cType          AS CHARACTER FORMAT "!" INIT "A" NO-UNDO.
    
    DEFINE BUFFER xbin    FOR fg-bin.
    DEFINE BUFFER xbin2   FOR fg-bin.
    DEFINE BUFFER b-rcpth FOR fg-rcpth.
    DEFINE BUFFER b-rdtlh FOR fg-rdtlh.
    
    ASSIGN
        cType          = SUBSTR(cItemCode2,1,1)
        lCustOwn       = lIncludeCustOwn
        lZeroQtyOnHand = lIncludeZeroQtyOnHand
        .    
    EMPTY TEMP-TABLE tt-cust.    
    IF lCustList THEN
        RUN pBuildCustList (
            cCompany,
            "IR4",
            OUTPUT cStartCustNo,
            OUTPUT cEndCustNo,
            OUTPUT lCustList
            ).
    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no GE cStartCustNo
          AND cust.cust-no LE cEndCustNo
          AND (IF lCustList THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
          AND ttCustList.log-fld) ELSE TRUE),
        EACH itemfg NO-LOCK
        WHERE itemfg.company    EQ cCompany
          AND itemfg.cust-no    EQ cust.cust-no
          AND itemfg.cust-po-no GE cStartCustPoNo
          AND itemfg.cust-po-no LE cEndCustPoNo
        :
        RUN fg/fgSlsRep.p (INPUT itemfg.company,
            INPUT itemfg.cust-no,
            INPUT itemfg.part-no,
            INPUT itemfg.i-no,
            OUTPUT cSlsRep).            
        IF lCustList AND
            NOT CAN-FIND(FIRST ttCustList
                         WHERE ttCustList.cust-no EQ cust.cust-no
                           AND ttCustList.log-fld EQ TRUE) THEN
        NEXT.            
        IF  cSlsRep GE cStartSalesRep AND
            cSlsRep LE cEndSalesRep  THEN DO:
            FIND FIRST tt-cust
                 WHERE tt-cust.cust-no EQ cust.cust-no
                 NO-ERROR.
            IF NOT AVAILABLE tt-cust THEN DO:
                CREATE tt-cust.
                ASSIGN 
                    tt-cust.cust-no  = cust.cust-no  
                    tt-cust.cust-row = ROWID(cust)
                    tt-cust.slsRep   = cSlsRep.
            END.                
        END.
    END. /* FOR EACH cust NO-LOCK */
    FOR EACH tt-cust,
        FIRST cust NO-LOCK
        WHERE ROWID(cust) EQ tt-cust.cust-row
        BREAK BY cust.cust-no
        :    
        {custom/statusMsg.i "'Processing Customer # ' + cust.cust-no"}    
        IF FIRST-OF(cust.cust-no) THEN
        dSellprice = 0.    
        FOR EACH itemfg NO-LOCK
            WHERE itemfg.company  EQ cCompany
              AND itemfg.cust-no EQ cust.cust-no
              AND itemfg.cust-po-no GE cStartCustPoNo
              AND itemfg.cust-po-no LE cEndCustPoNo
            BY itemfg.cust-no
            BY itemfg.i-no
            :
            RUN fg/fgSlsRep.p (
                itemfg.company,
                itemfg.cust-no,
                itemfg.part-no,
                itemfg.i-no,
                OUTPUT cSlsRep
                ).                
            IF NOT (cSlsRep GE cStartSalesRep AND
                    cSlsRep LE cEndSalesRep) THEN
            NEXT.              
            IF ((cType NE "A") AND (cType NE itemfg.i-code)) OR cType EQ "A" THEN DO:
                FOR EACH fg-bin NO-LOCK
                    WHERE fg-bin.company EQ cCompany
                      AND fg-bin.i-no    EQ itemfg.i-no
                    USE-INDEX co-ino
                    BREAK BY fg-bin.loc
                          BY fg-bin.job-no
                          BY fg-bin.job-no2
                    :    
                    IF (fg-bin.loc EQ "CUST" OR TRIM(fg-bin.cust-no) GT "") AND NOT lCustOwn THEN
                    NEXT.
                    ELSE
                    dQtyOnhand = dQtyOnhand + fg-bin.qty.    
                    IF LAST-OF(fg-bin.loc) THEN DO:
                        IF (dQtyOnhand NE 0 ) OR (dQtyOnhand EQ 0 AND lZeroQtyOnHand) THEN DO:
                            IF itemfg.sell-uom = "CS" AND itemfg.case-count NE 0 THEN
                            dExt = (dQtyOnhand * itemfg.sell-price) / itemfg.case-count.
                            ELSE
                                FIND FIRST uom NO-LOCK
                                     WHERE uom.uom EQ itemfg.sell-uom
                                       AND uom.mult NE 0
                                     NO-ERROR.
                            dExt = IF AVAILABLE uom THEN dQtyOnhand * itemfg.sell-price / uom.mult
                                   ELSE dQtyOnhand * itemfg.sell-price / 1000.    
                            IF itemfg.sell-uom = "L" THEN
                            dExt = itemfg.sell-price.
                            bin-block:
                            FOR EACH xbin NO-LOCK
                                WHERE xbin.company EQ cCompany
                                  AND xbin.i-no    EQ itemfg.i-no
                                  AND xbin.loc     EQ fg-bin.loc
                                BREAK BY xbin.job-no
                                      BY xbin.job-no2
                                :
                                IF FIRST-OF(xbin.job-no) OR first-of(xbin.job-no2) THEN
                                ASSIGN
                                    dQtyJob = 0
                                    dExtJob = 0
                                    .
                                dQtyJob = dQtyJob + xbin.qty.    
                                IF LAST-OF(xbin.job-no) OR LAST-OF(xbin.job-no2) THEN DO:
                                    FIND FIRST xbin2 NO-LOCK
                                         WHERE xbin2.company  EQ cCompany
                                           AND xbin2.i-no     EQ itemfg.i-no
                                           AND xbin2.loc      EQ fg-bin.loc
                                           AND (xbin2.job-no  NE xbin.job-no
                                            OR  xbin2.job-no2 NE xbin.job-no2)
                                           AND xbin2.qty NE 0
                                         NO-ERROR.
                                    IF AVAILABLE xbin2 AND dQtyJob EQ 0 THEN
                                    NEXT.
                                    IF itemfg.sell-uom EQ "CS" AND
                                       itemfg.case-count NE 0 THEN
                                    dExtJob = (dQtyJob * itemfg.sell-price) / itemfg.case-count.
                                    ELSE
                                        FIND FIRST uom NO-LOCK
                                             WHERE uom.uom  EQ itemfg.sell-uom
                                               AND uom.mult NE 0
                                             NO-ERROR.
                                    dExtJob = IF AVAILABLE uom THEN dQtyJob * itemfg.sell-price / uom.mult
                                              ELSE dQtyJob * itemfg.sell-price / 1000.    
                                    IF itemfg.sell-uom = "L" THEN
                                    dExtJob = itemfg.sell-price.                          
                                    ASSIGN
                                        dtTransDate = ?
                                        cLotNum     = ""
                                        .
                                    IF AVAILABLE xbin THEN
                                        IF TRIM(xbin.tag) NE "" THEN
                                            FOR EACH b-rdtlh NO-LOCK
                                                WHERE b-rdtlh.company   EQ xbin.company
                                                  AND b-rdtlh.tag       EQ xbin.tag
                                                  AND b-rdtlh.rita-code EQ "R"
                                                USE-INDEX tag,
                                                EACH b-rcpth NO-LOCK
                                                WHERE b-rcpth.r-no      EQ b-rdtlh.r-no
                                                  AND b-rcpth.i-no      EQ xbin.i-no
                                                  AND b-rcpth.rita-code EQ b-rdtlh.rita-code 
                                                USE-INDEX r-no
                                                BY b-rcpth.trans-date
                                                BY b-rcpth.r-no
                                                :
                                                LEAVE.
                                            END.
                                        ELSE
                                            IF TRIM(xbin.job-no) NE "" THEN
                                                FOR EACH b-rcpth NO-LOCK
                                                    WHERE b-rcpth.company   EQ xbin.company
                                                      AND b-rcpth.job-no    EQ xbin.job-no
                                                      AND b-rcpth.job-no2   EQ xbin.job-no2
                                                      AND b-rcpth.i-no      EQ xbin.i-no
                                                      AND b-rcpth.rita-code EQ "R"
                                                    USE-INDEX job,
                                                    EACH b-rdtlh NO-LOCK
                                                    WHERE b-rdtlh.r-no      EQ b-rcpth.r-no
                                                      AND b-rdtlh.rita-code EQ b-rcpth.rita-code
                                                    BY b-rcpth.trans-date
                                                    BY b-rcpth.r-no
                                                    :
                                                    LEAVE.
                                                END.    
                                    IF AVAILABLE b-rcpth THEN
                                    dtTransDate = b-rcpth.trans-date.
                                    IF AVAILABLE b-rdtlh AND b-rdtlh.stack-code NE "" AND cLotNum EQ "" THEN
                                    cLotNum = STRING(b-rdtlh.stack-code,"x(20)").    
                                    cJobNo = IF xbin.job-no EQ "" AND xbin.job-no2 = 0 THEN ""
                                             ELSE xbin.job-no + "-" + string(xbin.job-no2,"999").    
                                    FIND FIRST oe-ordl NO-LOCK
                                         WHERE oe-ordl.company EQ cCompany
                                           AND oe-ordl.job-no  EQ xbin.job-no
                                           AND oe-ordl.job-no2 EQ xbin.job-no2
                                           AND oe-ordl.i-no    EQ xbin.i-no
                                        USE-INDEX job
                                        NO-ERROR.    
                                    IF AVAILABLE oe-ordl THEN
                                    dExtJob = (oe-ordl.t-price / oe-ordl.qty) * dQtyJob.    
                                    iOrderNo = IF AVAILABLE oe-ordl THEN oe-ordl.ord-no ELSE 0.    
                                    FIND FIRST oe-ord NO-LOCK 
                                         WHERE oe-ord.company    EQ cCompany
                                           AND oe-ord.ord-no     EQ iOrderNo
                                           AND oe-ord.csrUser_id GE cStartUserID
                                           AND oe-ord.csrUser_id LE cEndUserID
                                           AND oe-ord.ord-no     NE 0
                                        NO-ERROR.    
                                    IF NOT AVAILABLE oe-ord THEN
                                    NEXT bin-block.    
                                    IF AVAILABLE oe-ordl THEN
                                        FIND FIRST job NO-LOCK
                                             WHERE job.company EQ oe-ordl.company
                                               AND job.job-no  EQ oe-ordl.job-no
                                               AND job.job-no2 EQ oe-ordl.job-no2
                                             NO-ERROR.    
                                    cSalesRep = "".    
                                    RUN fg/fgSlsRep.p (
                                        itemfg.company,
                                        itemfg.cust-no,
                                        itemfg.part-no,
                                        itemfg.i-no,
                                        OUTPUT cSalesRep
                                        ).
                                    ASSIGN
                                        dSellprice  = IF AVAILABLE oe-ordl THEN (oe-ordl.t-price / oe-ordl.qty * 1000)
                                                      ELSE itemfg.sell-price
                                        iCommited   = 0
                                        cCustLotNum = ""
                                        .
                                    FOR EACH oe-rel NO-LOCK
                                        WHERE oe-rel.company EQ oe-ordl.company
                                          AND oe-rel.ord-no  EQ oe-ordl.ord-no
                                          AND oe-rel.i-no    EQ oe-ordl.i-no 
                                          AND oe-rel.LINE    EQ oe-ordl.line
                                        :
                                        IF oe-rel.link-no EQ 0 THEN
                                        iCommited = iCommited + oe-rel.tot-qty.                            
                                        IF oe-rel.lot-no NE "" AND cCustLotNum EQ "" THEN
                                        cCustLotNum = STRING(oe-rel.lot-no,"x(15)"). 
                                    END.                           
                                    CREATE ttFGInventoryStatus.
                                    ASSIGN
                                        ttFGInventoryStatus.cCustNo     = STRING(cust.cust-no)
                                        ttFGInventoryStatus.cPo         = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.po-no,"x(15)") ELSE STRING(itemfg.cust-po-no,"x(15)")
                                        ttFGInventoryStatus.cSman       = STRING(cSalesRep)
                                        ttFGInventoryStatus.cItemNo     = STRING(itemfg.i-no,"x(15)")
                                        ttFGInventoryStatus.cPartNo     = STRING(itemfg.part-no,"x(15)")
                                        ttFGInventoryStatus.cItemName   = STRING(itemfg.i-name,"x(15)")
                                        ttFGInventoryStatus.cJobNo      = cJobNo
                                        ttFGInventoryStatus.iQtyOnHand  = INTEGER(dQtyJob)
                                        ttFGInventoryStatus.cRcptDate   = IF NOT(lZeroQtyOnHand AND dQtyOnhand = 0) AND NOT(dQtyJob = 0) AND dtTransDate NE ? THEN STRING(dtTransDate) ELSE ""
                                        ttFGInventoryStatus.dSellPrice  = dSellprice
                                        ttFGInventoryStatus.dTotalValue = dExtJob
                                        ttFGInventoryStatus.iCommitted  = iCommited
                                        ttFGInventoryStatus.iQtyCase    = itemfg.case-count
                                        ttFGInventoryStatus.cFgLot      = cLotNum
                                        ttFGInventoryStatus.cCustLot    = cCustLotNum
                                        ttFGInventoryStatus.cOrdDueDate = IF AVAILABLE oe-ord AND oe-ord.due-date NE ? THEN STRING(oe-ord.due-date,"99/99/9999") ELSE ""
                                        ttFGInventoryStatus.cJobDueDate = IF AVAILABLE job AND job.due-date NE ? THEN STRING(job.due-date,"99/99/9999") ELSE ""
                                        ttFGInventoryStatus.cCSR        = IF AVAILABLE oe-ord AND oe-ord.csrUser_id NE "" THEN STRING(oe-ord.csrUser_id,"x(8)") ELSE ""
                                        iCount                          = iCount + 1
                                        .    
                                END. /* if last-of(... */
                            END. /* for each xbin */    
                        END. /* qty onh */
                    END. /* last of bin */
                END. /* for each bin */
            END. /* item type */
        END.  /* for each item */           
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?). 
    END.  /* for each cust */ 
 
END PROCEDURE.

{AOA/dynBL/pBuildCustList.i}
