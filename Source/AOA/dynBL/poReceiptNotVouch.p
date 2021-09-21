/*------------------------------------------------------------------------
  File:         AOA/dynBL/poReceiptNotVouch.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 9.21.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttPoReceipt
DEFINE TEMP-TABLE ttPoReceipt NO-UNDO
    FIELD po-no       AS INTEGER   FORMAT ">>>>>>>>"   LABEL "PO#"
    FIELD po-line     AS INTEGER   FORMAT ">>>>"       LABEL "PO Line"
    FIELD cItem       AS CHARACTER FORMAT "x(15)"      LABEL "Item"     
    FIELD rec-date    AS DATE      FORMAT "99/99/9999" LABEL "Received Date"
    FIELD ord-qty     AS INTEGER   FORMAT ">>>>>>>>9"  LABEL "PO Order Quantity"
    FIELD qty-uom     AS CHARACTER FORMAT "x(3)"       LABEL "PO Order UOM"
    FIELD rec-qty     AS INTEGER   FORMAT ">>>>>>>>9"  LABEL "PO Received Qty"
    FIELD rec-qty-uom AS CHARACTER FORMAT "x(3)"       LABEL "PO Received UOM"     
    FIELD vend-no     AS CHARACTER FORMAT "x(8)"       LABEL "Vendor"
    FIELD actnum      AS CHARACTER FORMAT "x(15)"      LABEL "GL Account"
    FIELD descr       AS CHARACTER FORMAT "x(30)"      LABEL "Description"
    FIELD prod-cat    AS CHARACTER FORMAT "x(8)"       LABEL "Category"    
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 182
{AOA/includes/subjectID{&subjectID}Defs.i}

{custom/globdefs.i}

/* Local Variable Definitions ---                                       */

DEFINE NEW SHARED VARIABLE cocode AS CHARACTER FORMAT "x(3)" NO-UNDO.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic: 
    DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cRecUom   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate    AS DATE      NO-UNDO.
    DEFINE VARIABLE iQtyRec   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dtAmtRec  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iQtyInv   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dtAmtInv  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cVendNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProcat   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCountPro AS INTEGER   NO-UNDO.
    
    cocode = cCompany.        
    MAIN-LOOP:
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company   EQ cCompany
          AND (po-ordl.po-no    GE integer(cStartPONumber) OR lAllPO EQ YES)
          AND (po-ordl.po-no    LE integer(cEndPONumber)   OR lAllPO EQ YES)
          AND po-ordl.t-rec-qty GT 0
          AND po-ordl.excludeFromVoucher EQ NO
        USE-INDEX opened,
        FIRST po-ord NO-LOCK
        WHERE po-ord.company EQ cCompany
          AND po-ord.po-no   EQ po-ordl.po-no
          AND po-ord.po-date GE dtStartPoDate
          AND po-ord.po-date LE dtEndPoDate           
          AND po-ord.excludeFromVoucher EQ NO
        BREAK BY po-ord.po-no         
        :        
        IF po-ordl.item-type AND
            NOT CAN-FIND(FIRST rm-rcpth
                         WHERE rm-rcpth.company        EQ cCompany
                           AND rm-rcpth.i-no       EQ po-ordl.i-no
                           AND rm-rcpth.po-no      EQ string(po-ordl.po-no)
                           AND rm-rcpth.job-no     EQ po-ordl.job-no
                           AND rm-rcpth.job-no2    EQ po-ordl.job-no2
                           AND rm-rcpth.trans-date GE dtStartReceiptDate
                           AND rm-rcpth.trans-date LE dtEndReceiptDate
                           AND rm-rcpth.rita-code  EQ "R") THEN
        NEXT MAIN-LOOP.
        ELSE IF NOT po-ordl.item-type AND
                NOT CAN-FIND(FIRST fg-rcpth
                             WHERE fg-rcpth.company    EQ cCompany
                               AND fg-rcpth.i-no       EQ po-ordl.i-no
                               AND fg-rcpth.po-no      EQ string(po-ordl.po-no)
                               AND fg-rcpth.rita-code  EQ "R"
                               AND fg-rcpth.trans-date GE dtStartReceiptDate
                               AND fg-rcpth.trans-date LE dtEndReceiptDate) THEN
             NEXT MAIN-LOOP.        
        ASSIGN
            dtDate   = 01/01/0001
            iQtyRec  = 0
            dtAmtRec = 0
            iQtyInv  = 0
            dtAmtInv = 0
            cVendNo  = po-ord.vend-no
            cRecUom  = ""
            .        
        IF po-ordl.item-type THEN DO:
            FIND FIRST item NO-LOCK
                WHERE item.company EQ cCompany
                  AND item.i-no    EQ po-ordl.i-no
                NO-ERROR.
            cProcat = IF AVAILABLE item THEN item.procat ELSE "".

            FOR EACH rm-rcpth FIELDS(trans-date r-no pur-uom) NO-LOCK
                WHERE rm-rcpth.company    EQ cCompany
                  AND rm-rcpth.i-no       EQ po-ordl.i-no
                  AND rm-rcpth.po-no      EQ string(po-ordl.po-no)
                  AND rm-rcpth.job-no     EQ po-ordl.job-no
                  AND rm-rcpth.job-no2    EQ po-ordl.job-no2
                  AND rm-rcpth.trans-date GE dtStartReceiptDate
                  AND rm-rcpth.trans-date LE dtEndReceiptDate
                  AND rm-rcpth.rita-code  EQ "R"
                USE-INDEX item-po,
                FIRST rm-rdtlh NO-LOCK
                WHERE rm-rdtlh.r-no   EQ rm-rcpth.r-no
                  AND rm-rdtlh.s-num  EQ po-ordl.s-num
                   BY rm-rcpth.trans-date DESCENDING
                :
                ASSIGN
                    dtDate = rm-rcpth.trans-date
                    cRecUom = rm-rcpth.pur-uom
                    .
                RUN sys/inc/po-recqa2.p (
                    RECID(po-ordl),
                    dtStartReceiptDate,
                    dtEndReceiptDate,
                    OUTPUT iQtyRec,
                    OUTPUT dtAmtRec
                    ).
                LEAVE.
            END.
        END.
        ELSE DO:
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ cCompany
                  AND itemfg.i-no    EQ po-ordl.i-no
                NO-ERROR.
            cProcat = IF AVAILABLE itemfg THEN itemfg.procat ELSE "".
            FOR EACH fg-rcpth FIELDS(trans-date pur-uom) NO-LOCK
                WHERE fg-rcpth.company    EQ cCompany
                  AND fg-rcpth.i-no       EQ po-ordl.i-no
                  AND fg-rcpth.po-no      EQ string(po-ordl.po-no)
                  AND fg-rcpth.rita-code  EQ "R"
                  AND fg-rcpth.trans-date GE dtStartReceiptDate
                  AND fg-rcpth.trans-date LE dtEndReceiptDate
                USE-INDEX item-po
                :
                ASSIGN
                    dtDate = fg-rcpth.trans-date
                    cRecUom = fg-rcpth.pur-uom
                    .
                RUN sys/inc/po-recqa2.p (
                    RECID(po-ordl),
                    dtStartReceiptDate,
                    dtEndReceiptDate,
                    OUTPUT iQtyRec,
                    OUTPUT dtAmtRec
                    ).
                LEAVE.
            END.
        END.                
        IF dtAmtRec NE 0 THEN DO:
            RUN sys/inc/po-invqa.p (RECID(po-ordl), OUTPUT iQtyInv, OUTPUT dtAmtInv).
            IF iQtyRec - iQtyInv GT 0 THEN DO:
                IF (dtAmtRec GT dtAmtInv AND dtAmtRec GT 0) OR
                   (dtAmtRec LT dtAmtInv AND dtAmtRec LT 0) THEN DO:
                    CREATE ttPoReceipt.
                    ASSIGN
                        ttPoReceipt.vend-no     = cVendNo
                        ttPoReceipt.po-no       = po-ordl.po-no
                        ttPoReceipt.po-line     = po-ordl.LINE
                        ttPoReceipt.actnum      = po-ordl.actnum
                        ttPoReceipt.rec-date    = dtDate
                        ttPoReceipt.cItem       = po-ordl.i-no
                        ttPoReceipt.descr       = po-ordl.i-name  
                        ttPoReceipt.prod-cat    = cProcat  
                        ttPoReceipt.ord-qty     = po-ordl.ord-qty
                        ttPoReceipt.qty-uom     = po-ordl.pr-uom                          
                        ttPoReceipt.rec-qty     = iQtyRec
                        ttPoReceipt.rec-qty-uom = cRecUom
                        .                        
                        IF LAST-OF(po-ord.po-no) THEN
                        RUN pRunAPIOutboundTrigger(BUFFER po-ord).                    
                END.
            END.
        END.     
        iCountPro = iCountPro + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCountPro, ?).        
    END. 
    
END PROCEDURE.

PROCEDURE pRunAPIOutboundTrigger PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE PARAMETER BUFFER ipbf-po-ord FOR po-ord.

    DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.
                
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.                            
    RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
        INPUT  ipbf-po-ord.company,                      /* Company Code (Mandatory) */
        INPUT  ipbf-po-ord.loc,                          /* Location Code (Mandatory) */
        INPUT  "SendPurchaseOrder",                      /* API ID (Mandatory) */
        INPUT  ipbf-po-ord.vend-no,                      /* Scope ID */
        INPUT  "Vendor",                                 /* Scoped Type */
        INPUT  "QuantityReceived",                       /* Trigger ID (Mandatory) */
        INPUT  "po-ord",                                 /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  STRING(ROWID(ipbf-po-ord)),               /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  STRING(ipbf-po-ord.po-no),                /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Triggered from PO Qty QuantityReceived", /* Event's description (Optional) */
        OUTPUT lSuccess,                                 /* Success/Failure flag */
        OUTPUT cMessage                                  /* Status message */
        ) NO-ERROR.        
            
    DELETE PROCEDURE hdOutboundProcs.  

END PROCEDURE.
