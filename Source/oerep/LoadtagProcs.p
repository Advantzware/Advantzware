
/* **********************  Internal Procedures  *********************** */
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.
DEFINE VARIABLE list-name      AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img1   AS CHARACTER NO-UNDO FORMAT "x(200)".
DEFINE VARIABLE ls-image1      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCountPallet   AS INTEGER   NO-UNDO.

DEFINE VARIABLE glUpdateSetWithMaxQuantity             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glUpdateLoadTagSSCC                    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glUpdateLocBinFromItemFG               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glUpdateLocBinFromFGBin                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCreateFGReceipts                     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCheckClosedStatus                    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCreateRFIDTag                        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCreateComponenetTagsForSetHeaderItem AS LOGICAL   NO-UNDO.
DEFINE VARIABLE giFGSetRec                             AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcLoadTag                              AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcLabelMatrixLoadTagOutputFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcLabelMatrixLoadTagOutputPath         AS CHARACTER NO-UNDO.

DEFINE VARIABLE iTagCounter AS INTEGER NO-UNDO.

{oerep/ttLoadTag.i SHARED}
{fg/fullset.i NEW}
{oerep/r-loadtg.i }
{custom/xprint.i}

{sys/inc/var.i shared}
{sys/form/r-top3.f}

 DEFINE SHARED TEMP-TABLE tt-word-print LIKE w-ord 
   FIELD tag-no AS CHARACTER .
 
DEFINE TEMP-TABLE ttFGRctd NO-UNDO
    FIELD fgRctdROWID AS ROWID
    FIELD isComponent AS LOGICAL
    .
  
RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.  
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "LoadTagXprintImage",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT ls-image1,
                       OUTPUT lFound).

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

RUN pUpdateConfig.

/* ************************  Function Prototypes ********************** */
FUNCTION fGetNextTTLoadTagRecordID RETURNS INTEGER PRIVATE
	(  ) FORWARD.

FUNCTION fReplaceQuotes RETURNS CHARACTER PRIVATE
	( INPUT ipcField AS CHARACTER ) FORWARD.

PROCEDURE CreateLoadTagFromTT:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iTagCount AS INTEGER NO-UNDO. 
      
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:    
        RUN pPrintLoadTagHeader.
        
        FOR EACH ttLoadTag
            WHERE ttLoadTag.tagStatus EQ "Created":
            /* v-loadtag - additional code is required to calculate totaltags to print using LOADTAG NK1 */
            DO iTagCount = 1 TO ttLoadTag.totalTags:
                iTagCounter = iTagCounter + 1.
                RUN pCreateLoadTagFromTT (
                    BUFFER ttLoadTag,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).
            END.
            
            ttLoadTag.tagStatus = "Printed".
        END.
        
        iTagCounter = 0.
    END.
END PROCEDURE.

PROCEDURE pCreateLoadTagFromTT:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttLoadTag FOR ttLoadTag.
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE cRfidTag              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNextRNo              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-full-qty           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-use-full-qty       AS LOG       NO-UNDO.
    DEFINE VARIABLE dCostPerUOM           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cNextLoadtag          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdPOProcs             AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdJobProcs            AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE dMaxQty               AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-loadtag  FOR loadtag.
    DEFINE BUFFER bf-po-ordl  FOR po-ordl.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER bf-fg-bin   FOR fg-bin.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-job      FOR job.
    DEFINE BUFFER bf-job-hdr  FOR job-hdr.
    DEFINE BUFFER bf-reftable FOR reftable.
    DEFINE BUFFER bf-rfidtag  FOR rfidtag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        IF NOT AVAILABLE ipbf-ttLoadTag THEN DO:
            ASSIGN
                oplError  = TRUE
                opcMessage = "Record not found for creating loadtag"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.    
        END.
        
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ ipbf-ttLoadTag.company
               AND bf-itemfg.i-no    EQ ipbf-ttLoadTag.itemID
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN DO:
            ASSIGN
                oplError  = TRUE
                opcMessage = "Finished Good item # '" + ipbf-ttLoadTag.itemID + "' not found"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.    
        END.
        
        
        RUN loadtags\GetNextTag.p (
            INPUT  ipbf-ttLoadTag.company, 
            INPUT  ipbf-ttLoadTag.itemID,
            OUTPUT cNextLoadtag
            ).    
    
        IF glUpdateLoadTagSSCC THEN
            RUN oerep/ldtagSSCC.p (
                INPUT  ipbf-ttLoadTag.company,
                INPUT  ipbf-ttLoadTag.custID,
                OUTPUT ipbf-ttLoadTag.SSCC
                ).
    
        CREATE bf-loadtag.
        ASSIGN
            bf-loadtag.company      = ipbf-ttLoadTag.company
            bf-loadtag.tag-no       = cNextLoadtag 
            bf-loadtag.item-type    = NO /*FGitem*/
            bf-loadtag.job-no       = ipbf-ttLoadTag.jobID
            bf-loadtag.job-no2      = ipbf-ttLoadTag.jobID2
            bf-loadtag.ord-no       = IF CAN-FIND(FIRST cust 
                                                  WHERE cust.company = cocode
                                                    AND cust.cust-no = bf-itemfg.cust-no
                                                    AND cust.active = "X") THEN 
                                          0
                                      ELSE 
                                          ipbf-ttLoadTag.orderID
            bf-loadtag.i-no         = CAPS(ipbf-ttLoadTag.itemID)
            bf-loadtag.i-name       = ipbf-ttLoadTag.itemName
            bf-loadtag.qty          = ipbf-ttLoadTag.ordQuantity
            bf-loadtag.qty-case     = ipbf-ttLoadTag.pcs
            bf-loadtag.case-bundle  = ipbf-ttLoadTag.bundle
            bf-loadtag.pallet-count = ipbf-ttLoadTag.totalUnit
            bf-loadtag.partial      = ipbf-ttLoadTag.partial
            bf-loadtag.sts          = "Printed"
            bf-loadtag.tag-date     = TODAY
            bf-loadtag.tag-time     = TIME
            bf-loadtag.misc-dec[1]  = ipbf-ttLoadTag.unitWeight 
            bf-loadtag.misc-dec[2]  = ipbf-ttLoadTag.palletWeight
            bf-loadtag.misc-char[2] = ipbf-ttLoadTag.lot
            bf-loadtag.spare-char-1 = ipbf-ttLoadTag.SSCC
            bf-loadtag.pallet-no    = bf-itemfg.trno
            .
        
        IF glUpdateLocBinFromItemFG THEN
            ASSIGN
                bf-loadtag.loc     = bf-itemfg.def-loc
                bf-loadtag.loc-bin = bf-itemfg.def-loc-bin
                .
        ELSE IF glUpdateLocBinFromFGBin THEN DO:
            FIND FIRST bf-fg-bin NO-LOCK
                 WHERE bf-fg-bin.company EQ bf-itemfg.company
                   AND bf-fg-bin.i-no    EQ bf-itemfg.i-no
                   AND bf-fg-bin.job-no  EQ ipbf-ttLoadTag.jobID
                   AND bf-fg-bin.tag     EQ bf-loadtag.tag-no
                 NO-ERROR.
            IF AVAILABLE bf-fg-bin THEN
                ASSIGN
                    bf-loadtag.loc     = bf-fg-bin.loc
                    bf-loadtag.loc-bin = bf-fg-bin.loc-bin
                    .

        END.
        ELSE
            RUN fg/autopost.p ( 
                INPUT  ROWID(bf-itemfg), 
                INPUT  ipbf-ttLoadTag.jobID,
                INPUT  ipbf-ttLoadTag.jobID2,
                OUTPUT bf-loadtag.loc, 
                OUTPUT bf-loadtag.loc-bin
                ).
    
        IF glCreateRFIDTag THEN DO:
            RUN GetNextRFIDTag (
                INPUT  ipbf-ttLoadTag.company, 
                OUTPUT cRfidTag
                ).
    
            CREATE bf-rfidtag.
            ASSIGN 
                bf-rfidtag.company   = bf-loadtag.company
                bf-rfidtag.item-type = bf-loadtag.item-type
                bf-rfidtag.tag-no    = bf-loadtag.tag-no
                bf-rfidtag.rfidtag   = cRfidTag
                .
        END.
    
        IF glCreateFGReceipts AND glCheckClosedStatus THEN DO:
            RUN po/POProcs.p PERSISTENT SET hdPOProcs.
            
            RUN CheckPOLineStatus IN hdPoProcs (
                INPUT ipbf-ttLoadTag.company,
                INPUT ipbf-ttLoadTag.poID,
                INPUT ipbf-ttLoadTag.poLineID
                ) NO-ERROR.
            
            DELETE PROCEDURE hdPOProcs.
            
            IF ERROR-STATUS:ERROR THEN 
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
     
            RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
            
            RUN CheckJobStatus IN hdJobProcs (
                INPUT ipbf-ttLoadTag.company,
                INPUT ipbf-ttLoadTag.jobID,
                INPUT ipbf-ttLoadTag.jobID2
                ) NO-ERROR.
                
            DELETE PROCEDURE hdJobProcs.
            
            IF ERROR-STATUS:ERROR THEN
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK. 
        END.
        
        
        IF glCreateFGReceipts THEN DO:
            iNextRNo = 0.
            FIND LAST bf-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
            IF AVAILABLE bf-fg-rctd AND bf-fg-rctd.r-no GT iNextRNo THEN 
                iNextRNo = bf-fg-rctd.r-no.
    
            FIND LAST bf-fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAILABLE bf-fg-rcpth AND bf-fg-rcpth.r-no GT iNextRNo THEN
                iNextRNo = bf-fg-rcpth.r-no.
    
            DO WHILE TRUE:
                iNextRNo = iNextRNo + 1.
                FIND FIRST bf-fg-rcpth NO-LOCK
                     WHERE bf-fg-rcpth.r-no EQ iNextRNo 
                     USE-INDEX r-no NO-ERROR.
                IF AVAILABLE bf-fg-rcpth THEN
                    NEXT.
                    
                FIND FIRST bf-fg-rctd NO-LOCK
                     WHERE bf-fg-rctd.r-no EQ iNextRNo 
                     USE-INDEX fg-rctd NO-ERROR.
                IF AVAILABLE bf-fg-rctd THEN
                    NEXT.
                    
                LEAVE.
            END.
    
            CREATE bf-fg-rctd.
            ASSIGN
                bf-fg-rctd.r-no       = iNextRNo + 1
                bf-fg-rctd.rct-date   = TODAY
                bf-fg-rctd.trans-time = TIME
                bf-fg-rctd.company    = ipbf-ttLoadTag.company
                bf-fg-rctd.rita-code  = "R"
                bf-fg-rctd.i-name     = bf-itemfg.i-name
                bf-fg-rctd.i-no       = bf-loadtag.i-no
                bf-fg-rctd.job-no     = bf-loadtag.job-no
                bf-fg-rctd.job-no2    = bf-loadtag.job-no2
                bf-fg-rctd.t-qty      = bf-loadtag.pallet-count
                bf-fg-rctd.pur-uom    = bf-itemfg.prod-uom
                bf-fg-rctd.cost-uom   = bf-itemfg.prod-uom
                bf-fg-rctd.ext-cost   = (bf-fg-rctd.t-qty / 1000) * bf-fg-rctd.std-cost
                bf-fg-rctd.qty-case   = bf-loadtag.qty-case
                bf-fg-rctd.partial    = bf-loadtag.partial
                bf-fg-rctd.cases      = IF bf-loadtag.qty-case NE 0 THEN 
                                            TRUNCATE(bf-fg-rctd.t-qty / bf-loadtag.qty-case,0) 
                                        ELSE
                                            0
                bf-fg-rctd.cases-unit = bf-loadtag.case-bundle
                bf-fg-rctd.loc        = bf-loadtag.loc
                bf-fg-rctd.loc-bin    = bf-loadtag.loc-bin
                bf-fg-rctd.tag        = bf-loadtag.tag-no
                bf-fg-rctd.stack-code = bf-loadtag.misc-char[2]
                bf-fg-rctd.tot-wt     = bf-loadtag.misc-dec[1]
                .
    
            IF bf-fg-rctd.cases EQ 0 AND bf-fg-rctd.partial NE 0 THEN
                ASSIGN 
                    bf-fg-rctd.cases    = (IF bf-fg-rctd.partial LT 0 THEN -1 ELSE 1)
                    bf-fg-rctd.qty-case = (IF bf-fg-rctd.partial LT 0 THEN - bf-fg-rctd.partial ELSE bf-fg-rctd.partial)
                    bf-fg-rctd.partial  = 0
                    .
    
            CREATE ttFGRctd.
            ASSIGN 
                ttFGRctd.fgRctdROWID = ROWID(bf-fg-rctd)
                ttFGRctd.isComponent = ipbf-ttLoadTag.isComponent
                .
    
            IF ipbf-ttLoadTag.recordSource EQ "PO" THEN DO:
                ASSIGN
                    bf-fg-rctd.po-no   = TRIM(STRING(bf-loadtag.po-no,">>>>>>>>>>"))
                    bf-fg-rctd.po-line = bf-loadtag.line
                    .
    
                IF bf-loadtag.po-no GT 0 THEN
                    ASSIGN
                        bf-fg-rctd.job-no  = ""
                        bf-fg-rctd.job-no2 = 0
                        .
            END.
    
            IF TRIM(bf-fg-rctd.job-no) NE "" THEN
                FIND FIRST bf-job NO-LOCK
                     WHERE bf-job.company EQ bf-fg-rctd.company
                       AND bf-job.job-no  EQ bf-fg-rctd.job-no
                       AND bf-job.job-no2 EQ bf-fg-rctd.job-no2
                     USE-INDEX job 
                     NO-ERROR.
    
            IF AVAILABLE bf-job THEN DO:
                FIND FIRST bf-job-hdr NO-LOCK
                     WHERE bf-job-hdr.company EQ bf-job.company
                       AND bf-job-hdr.job-no  EQ bf-loadtag.job-no
                       AND bf-job-hdr.job-no2 EQ bf-loadtag.job-no2
                       AND bf-job-hdr.i-no    EQ bf-itemfg.i-no
                     NO-ERROR.
                IF AVAILABLE bf-job-hdr THEN 
                    bf-fg-rctd.std-cost = bf-job-hdr.std-tot-cost.
                ELSE
                    FIND FIRST bf-reftable NO-LOCK 
                         WHERE bf-reftable.reftable EQ "jc/jc-calc.p"
                           AND bf-reftable.company  EQ bf-job.company
                           AND bf-reftable.loc      EQ ""
                           AND bf-reftable.code     EQ STRING(bf-job.job,"999999999")
                           AND bf-reftable.code2    EQ bf-fg-rctd.i-no
                           USE-INDEX reftable
                         NO-ERROR.
    
                IF AVAILABLE bf-reftable AND bf-reftable.val[5] NE 0 THEN
                    bf-fg-rctd.std-cost = bf-reftable.val[5].
            END.
    
            IF NOT AVAILABLE bf-job-hdr AND NOT AVAILABLE bf-reftable THEN DO:
                bf-fg-rctd.std-cost = bf-itemfg.std-tot-cost.
                
                FIND FIRST bf-fg-bin NO-LOCK
                     WHERE bf-fg-bin.company EQ bf-itemfg.company
                       AND bf-fg-bin.i-no    EQ bf-itemfg.i-no
                       AND bf-fg-bin.job-no  EQ bf-loadtag.job-no
                       AND bf-fg-bin.job-no2 EQ bf-loadtag.job-no2 
                    NO-ERROR.
                IF AVAILABLE bf-fg-bin THEN 
                    bf-fg-rctd.std-cost = bf-fg-bin.std-tot-cost.
            END.
    
            IF NOT giFGSetRec EQ 1 THEN DO:
                IF bf-itemfg.isaset AND
                  (bf-itemfg.alloc EQ NO  OR
                  (bf-itemfg.alloc EQ YES AND /* fgrecpt-char NE "Manual" AND*/ TRIM(bf-fg-rctd.job-no) NE "")) THEN DO:
                    ASSIGN
                        bf-fg-rctd.t-qty = bf-fg-rctd.cases * bf-fg-rctd.qty-case + bf-fg-rctd.partial
                        dMaxQty          = bf-fg-rctd.t-qty
                        .
        
                    RUN fg/checksetb.p (
                        INPUT        ROWID(bf-itemfg),
                        INPUT        ROWID(bf-fg-rctd),
                        INPUT        bf-fg-rctd.job-no,
                        INPUT        INT(bf-fg-rctd.job-no2),
                        INPUT        bf-fg-rctd.loc,
                        INPUT-OUTPUT dMaxQty
                        ).
        
                    IF dMaxQty LT bf-fg-rctd.t-qty THEN DO:
                        IF dMaxQty GT 0 AND glUpdateSetWithMaxQuantity THEN DO:
                            ASSIGN
                                bf-fg-rctd.t-qty   = dMaxQty
                                bf-fg-rctd.cases   = TRUNC((dMaxQty - DECIMAL(bf-fg-rctd.partial)) / DECIMAL(bf-fg-rctd.qty-case),0)
                                bf-fg-rctd.partial = dMaxQty - (DECIMAL(bf-fg-rctd.cases) * DECIMAL(bf-fg-rctd.qty-case))
                                .
        
                            IF bf-fg-rctd.cases EQ 0 AND bf-fg-rctd.partial NE 0 THEN
                                ASSIGN 
                                    bf-fg-rctd.cases    = (IF bf-fg-rctd.partial LT 0 THEN -1 ELSE 1)
                                    bf-fg-rctd.qty-case = (IF bf-fg-rctd.partial LT 0 THEN - bf-fg-rctd.partial ELSE bf-fg-rctd.partial)
                                    bf-fg-rctd.partial  = 0
                                    .
                        END.
                        
                        IF NOT glUpdateSetWithMaxQuantity OR dMaxQty EQ 0 THEN DO:                   
                            ASSIGN
                                oplError   = TRUE
                                opcMessage = "The finished goods receipt was not created "
                                           + "for item # " + bf-itemfg.i-no + ", Job # " 
                                           + bf-fg-rctd.job-no + "-" + STRING(bf-fg-rctd.job-no2)
                                           + " due to insufficient component inventory"
                                .
                            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
                        END.
                    END.
                END.
            END.
            
            IF ipbf-ttLoadTag.poID NE 0 AND (ipbf-ttLoadTag.recordSource EQ "ORDER" OR ipbf-ttLoadTag.recordSource EQ "PO" OR ipbf-ttLoadTag.recordSource EQ "JOB") THEN DO:
                ASSIGN
                    bf-fg-rctd.po-no   = TRIM(STRING(ipbf-ttLoadTag.poID,">>>>>>>>>>"))
                    bf-fg-rctd.po-line = ipbf-ttLoadTag.poLineID
                    .
    
                FIND FIRST bf-po-ordl NO-LOCK
                     WHERE bf-po-ordl.company   EQ ipbf-ttLoadTag.company 
                       AND bf-po-ordl.po-no     EQ ipbf-ttLoadTag.poID
                       AND bf-po-ordl.item-type EQ NO
                       AND bf-po-ordl.i-no      EQ ipbf-ttLoadtag.itemID
                       AND (bf-po-ordl.line     EQ ipbf-ttLoadTag.poLineID OR ipbf-ttLoadTag.poLineID EQ 0)
                     NO-ERROR.
    
                IF AVAILABLE bf-po-ordl THEN DO:
                    IF bf-fg-rctd.po-line EQ 0 THEN 
                        bf-fg-rctd.po-line = 1.
                        
                    RUN pGetCostFromPO (
                        INPUT  bf-po-ordl.company, 
                        INPUT  bf-po-ordl.po-no, 
                        INPUT  bf-po-ordl.line, 
                        INPUT  bf-po-ordl.i-no, 
                        INPUT  bf-fg-rctd.t-qty,
                        OUTPUT bf-fg-rctd.std-cost, 
                        OUTPUT bf-fg-rctd.cost-uom, 
                        OUTPUT bf-fg-rctd.ext-cost, 
                        OUTPUT bf-fg-rctd.frt-cost
                        ).
                        
                    RUN Conv_ValueFromUOMtoUOM (
                        INPUT  ipbf-ttLoadTag.company, 
                        INPUT  bf-po-ordl.i-no, 
                        INPUT  "FG", 
                        INPUT  bf-fg-rctd.std-cost, 
                        INPUT  bf-fg-rctd.cost-uom, 
                        INPUT  bf-po-ordl.cons-uom, 
                        INPUT  0, 
                        INPUT  bf-po-ordl.s-len,
                        INPUT  bf-po-ordl.s-wid,
                        INPUT  bf-po-ordl.s-dep, 0, 
                        OUTPUT bf-fg-rctd.std-cost, 
                        OUTPUT lError, 
                        OUTPUT cMessage
                        ).
                        
                    ASSIGN 
                        bf-fg-rctd.pur-uom  = bf-po-ordl.cons-uom 
                        bf-fg-rctd.cost-uom = bf-po-ordl.cons-uom
                        .
                END.
            END.
            ELSE DO:
                /* Development in progress */
/*                RUN calc-ext-cost.*/
            END.
    
            IF ipbf-ttLoadTag.estID NE "" THEN DO:
                IF bf-itemfg.pur-man THEN 
                DO:
                    IF TRIM(bf-fg-rctd.job-no) NE "" AND TRIM(bf-fg-rctd.po-no) NE "" AND ipbf-ttLoadTag.recordSource EQ "PO" THEN
                        ASSIGN 
                            bf-fg-rctd.job-no  = "" 
                            bf-fg-rctd.job-no2 = 0
                            bf-fg-rctd.po-no   = TRIM(STRING(ipbf-ttLoadTag.poID,">>>>>>>>>>"))
                            bf-fg-rctd.po-line = ipbf-ttLoadTag.poLineID
                            .
                    ELSE IF TRIM(bf-fg-rctd.job-no) NE "" AND TRIM(bf-fg-rctd.po-no) NE "" AND (ipbf-ttLoadTag.recordSource EQ "ORDER" OR ipbf-ttLoadTag.recordSource EQ "JOB") THEN
                        ASSIGN
                            bf-fg-rctd.po-no   = ""
                            bf-fg-rctd.job-no  = bf-loadtag.job-no
                            bf-fg-rctd.job-no2 = bf-loadtag.job-no2
                            .
                END.
                ELSE IF NOT bf-itemfg.pur-man THEN DO:
                    IF TRIM(bf-fg-rctd.po-no) NE "" AND TRIM(bf-loadtag.job-no) NE "" AND (ipbf-ttLoadTag.recordSource EQ "ORDER" OR ipbf-ttLoadTag.recordSource EQ "JOB") THEN
                        ASSIGN 
                            bf-fg-rctd.po-no   = ""
                            bf-fg-rctd.job-no  = bf-loadtag.job-no
                            bf-fg-rctd.job-no2 = bf-loadtag.job-no2
                            .
                    ELSE IF TRIM(bf-fg-rctd.po-no) NE "" AND TRIM(bf-loadtag.job-no) NE "" AND ipbf-ttLoadTag.recordSource EQ "PO" THEN
                        ASSIGN 
                            bf-fg-rctd.job-no  = "" 
                            bf-fg-rctd.job-no2 = 0
                            bf-fg-rctd.po-no   = TRIM(STRING(ipbf-ttLoadTag.poID,">>>>>>>>>>"))
                            bf-fg-rctd.po-line = ipbf-ttLoadTag.poLineID
                            .
                END.
            END.

            IF NOT ( giFGSetRec EQ 1 AND bf-itemfg.alloc NE YES) THEN
                RUN fg/comprcpt.p (
                    INPUT ROWID(bf-fg-rctd)
                    ).
        END.
      
        RUN pPrintLoadTag (
            BUFFER ipbf-ttLoadTag,
            BUFFER bf-loadtag 
            ).  
        /* Update the other tags with this new quantity */
/*        IF AVAILABLE bf-fg-rctd AND lv-use-full-qty THEN*/
/*            RUN get-set-full-qty (                        */
/*                INPUT  fg-rctd.job-no,                    */
/*                INPUT  fg-rctd.job-no2,                   */
/*                INPUT  fg-rctd.i-no,                      */
/*                INPUT  0 /* new qty */,                   */
/*                INPUT  fg-rctd.std-cost /* cost to set */,*/
/*                OUTPUT lv-full-qty                        */
/*                ).                                        */
    END.
END PROCEDURE.

PROCEDURE GetNextRFIDTag:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRfidTag AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dRFIDTag AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-oe-ctrl FOR oe-ctrl.
    
    FIND FIRST bf-oe-ctrl EXCLUSIVE-LOCK
         WHERE bf-oe-ctrl.company EQ ipcCompany
         NO-ERROR.
         
    dRFIDTag = IF AVAILABLE bf-oe-ctrl AND bf-oe-ctrl.spare-char-1 <> "" THEN
                   DECIMAL(bf-oe-ctrl.spare-char-1)
               ELSE 
                   111110000000000000000001. 
    
    bf-oe-ctrl.spare-char-1 = STRING(dRFIDTag + 1).

    opcRfidTag = STRING(dRFIDTag).

END PROCEDURE.

PROCEDURE pCreateSetComponentsForTTLoadTagItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSetsCreated       AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-ttLoadTag     FOR ttLoadTag.
    DEFINE BUFFER bf-set-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-itemfg        FOR itemfg.
    DEFINE BUFFER bf-set-itemfg    FOR itemfg.
    
    FOR FIRST bf-ttLoadTag
        WHERE bf-ttLoadTag.recordID   EQ ipiTTLoadTagRecordID
          AND bf-ttLoadTag.dontRunSet EQ NO:
        
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ bf-ttLoadTag.company
               AND bf-itemfg.i-no    EQ bf-ttLoadTag.itemID
               AND bf-itemfg.isaset
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN
            RETURN.

        /* Populates temp-table tt-fg-set with set companents for a given itemfg */
        RUN fg/fullset.p (
            ROWID(bf-itemfg)
            ).
        
        FOR EACH tt-fg-set 
            WHERE tt-fg-set.part-no NE bf-ttLoadTag.itemID,
            FIRST bf-set-itemfg NO-LOCK
            WHERE bf-set-itemfg.company EQ tt-fg-set.company
              AND bf-set-itemfg.i-no    EQ tt-fg-set.part-no:
            CREATE bf-set-ttLoadTag.
            BUFFER-COPY bf-ttLoadTag EXCEPT bf-ttLoadTag.recordID TO bf-set-ttLoadTag.      
            ASSIGN
                bf-set-ttLoadTag.recordID    = fGetNextTTLoadTagRecordID()
                bf-set-ttLoadTag.itemID      = tt-fg-set.part-no
                bf-set-ttLoadTag.itemName    = bf-set-itemfg.i-name
                bf-set-ttLoadTag.isComponent = YES
                bf-set-ttLoadTag.ordQuantity = bf-ttLoadTag.ordQuantity * tt-fg-set.part-qty-dec
                bf-set-ttLoadTag.boxLen      = bf-set-itemfg.l-score[50]
                bf-set-ttLoadTag.boxWid      = bf-set-itemfg.w-score[50]
                bf-set-ttLoadTag.boxDep      = bf-set-itemfg.d-score[50].
                oplSetsCreated               = YES
                .
   
            RUN pUpdateTTLoadTagEstimateDetails (
                INPUT bf-set-ttLoadTag.company,
                INPUT bf-set-ttLoadTag.estID,
                INPUT bf-set-ttLoadTag.recordID
                ).
        END.

        oplSetsCreated = oplSetsCreated AND ( IF bf-itemfg.alloc EQ ? THEN FALSE ELSE bf-itemfg.alloc).
    END.

END PROCEDURE.

PROCEDURE pGetCostFromPO PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONumber         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty              AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM       AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalFreight AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dCostPerEA        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostFreight      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostFreightPerEA AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lFound            AS LOGICAL NO-UNDO.
    DEFINE VARIABLE hdCostProcs       AS HANDLE  NO-UNDO.
    
    RUN system/CostProcs.p PERSISTENT SET hdCostProcs.
    
    RUN GetCostForPOLine IN hdCostProcs (
        INPUT  ipcCompany, 
        INPUT  ipiPONumber, 
        INPUT  ipiPOLine, 
        INPUT  ipcFGItemID, 
        OUTPUT opdCostPerUOM, 
        OUTPUT opcCostUOM, 
        OUTPUT dCostFreight, 
        OUTPUT lFound
        ).
    
    dCostPerEA = DYNAMIC-FUNCTION('fConvertCostForItem':U IN hdCostProcs,
                                  INPUT ipcCompany, 
                                  INPUT ipcFGItemID, 
                                  INPUT "FG", 
                                  INPUT opdCostPerUOM, 
                                  INPUT opcCostUOM, 
                                  INPUT "EA", 
                                  INPUT 0, /*BasisWeight*/
                                  INPUT 0, /*Length override - leave as 0 if not in UI or on Order/PO*/
                                  INPUT 0, /*Width override - leave as 0 if not in UI or on Order/PO*/
                                  INPUT 0, /*Depth override - leave as 0 if not in UI or on Order/PO*/
                                  INPUT 0, /*Case Count override - leave as 0 if not in UI or on Order/PO*/
                                  INPUT ipdQty, /*Lot Quantity - leave as 0 if not in UI or on Order/PO*/
                                  INPUT "EA" /*Lot Quantity UOM - leave as "" if not in UI or on PO*/
                                  ).
    
    dCostFreightPerEA = DYNAMIC-FUNCTION('fConvertCostForItem':U IN hdCostProcs,
                                         INPUT ipcCompany, 
                                         INPUT ipcFGItemID, 
                                         INPUT "FG", 
                                         INPUT dCostFreight, 
                                         INPUT opcCostUOM, 
                                         INPUT "EA", 
                                         INPUT 0, /*BasisWeight*/
                                         INPUT 0, /*Length override - leave as 0 if not in UI or on Order/PO*/
                                         INPUT 0, /*Width override - leave as 0 if not in UI or on Order/PO*/
                                         INPUT 0, /*Depth override - leave as 0 if not in UI or on Order/PO*/
                                         INPUT 0, /*Case Count override - leave as 0 if not in UI or on Order/PO*/
                                         INPUT ipdQty, /*Lot Quantity - leave as 0 if not in UI or on Order/PO*/
                                         INPUT "EA" /*Lot Quantity UOM - leave as "" if not in UI or on PO*/
                                         ).
    
    ASSIGN 
        opdCostTotal        = ipdQty * dCostPerEA
        opdCostTotalFreight = ipdQty * dCostFreightPerEA
        .
    
/*    IF fgpofrt-log THEN*/
        opdCostTotal = opdCostTotal + opdCostTotalFreight.
    
    DELETE PROCEDURE hdCostProcs.
END PROCEDURE.


PROCEDURE pPrintLoadTag PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttLoadTag FOR ttLoadTag.
    DEFINE PARAMETER BUFFER ipbf-loadtag   FOR loadtag.
    
    DEFINE BUFFER bf-rfidTag FOR rfidtag.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-cust    FOR cust.
    
    OUTPUT TO VALUE (gcLabelMatrixLoadTagOutputPath + gcLabelMatrixLoadTagOutputFile) APPEND.
      
    IF AVAILABLE ipbf-ttLoadTag AND AVAILABLE ipbf-loadtag THEN DO:
        FIND FIRST bf-rfidtag OF ipbf-loadtag NO-LOCK NO-ERROR.
        
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ ipbf-loadtag.company
               AND bf-itemfg.i-no    EQ ipbf-loadtag.i-no
             NO-ERROR.
        
        FIND FIRST bf-cust NO-LOCK
             WHERE bf-cust.company EQ ipbf-loadtag.company
               AND bf-cust.cust-no EQ ipbf-ttLoadtag.custID
               NO-ERROR.
               
        PUT UNFORMATTED
        '"'  fReplaceQuotes(ipbf-ttLoadTag.custName)  '",'
        ipbf-ttLoadTag.orderID  ","
        '"'  IF ipbf-ttLoadTag.jobID EQ "" THEN STRING(ipbf-ttLoadTag.orderID) ELSE ipbf-ttLoadTag.jobID + "-" + STRING(ipbf-ttLoadTag.jobID2, "99")  '",'
        '"'  CAPS(fReplaceQuotes(ipbf-ttLoadTag.itemID))  FORM "x(15)" '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.custPartNo) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.custPONo)  '",'
        ipbf-ttLoadTag.pcs  ","
        ipbf-ttLoadTag.bundle  ","
        TRIM(STRING(ipbf-ttLoadTag.totalUnit, ">>>>>>>9")) ","
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipID)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipName)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipAddress1)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipAddress2)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipCity)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipState) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipCountry)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipZip)   '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldID)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldName)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldAddress1)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldAddress2)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldCity)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldState) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldCountry)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldZip)   '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.itemName) FORMAT "X(30)"  '",'
        '"'  ipbf-ttLoadTag.dueDate  '",'
        '"'  ipbf-ttLoadTag.relDate  '",'
        '"'  ipbf-ttLoadTag.upcNo FORMAT "x(20)" '",'
        '"'  ipbf-ttLoadTag.boxLen FORMAT ">>>9.99<<<" '",'
        '"'  ipbf-ttLoadTag.boxWid FORMAT ">>>9.99<<<" '",'
        '"'  ipbf-ttLoadTag.boxDep FORMAT ">>>9.99<<<" '",'
        '"'  ipbf-ttLoadTag.flute  '",'
        '"'  ipbf-ttLoadTag.test  '",'
        '"'  ipbf-ttLoadTag.vendor  '",'
        ipbf-ttLoadTag.grossWeight  ","
        ipbf-ttLoadTag.tareWeight  ","
        ipbf-ttLoadTag.netWeight  ","
        ipbf-ttLoadTag.sheetWeight  ","
        '"'  ipbf-ttLoadTag.uom  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.style) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.styleDesc) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.relLotID) '",'
        '"'  /* lv-middlesex-job */ '",'
        '"'  /* lv-middlesex-po */ '",'
        '"'  ipbf-loadtag.tag-no '",'
        '"'  ipbf-loadtag.partial '",'
        '"'  ipbf-ttLoadTag.caseNo  '",'
        '"'  /* fReplaceQuotes(v-dept-note[1]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[2]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[3]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[4]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[5]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[6]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[7]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[8]) */ '",'
        ipbf-ttLoadTag.poID ","
        '"'  /* fReplaceQuotes(v-dept-note[9])  */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[10]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[11]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[12]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[13]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[14]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[15]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[16]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[17]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[18]) */ '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.estID) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.orderDesc1)    '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.orderDesc2)    '",'
        .
        
        IF CAN-DO("ASI,SSLABEL",gcLoadTag) THEN DO:
        PUT UNFORMATTED
        '"' SUBSTR(ipbf-loadtag.tag-no,16,5) '",'
        '"' IF AVAILABLE bf-rfidtag THEN bf-rfidtag.rfidtag ELSE '' '",' .
        END.
        
        PUT UNFORMATTED
        '"' ipbf-ttLoadTag.dueDateJobhdr '",'
        '"' ipbf-ttLoadTag.dueDateJob '",'
        '"' ipbf-ttLoadTag.lineID '",'
        '"' ipbf-ttLoadTag.unitWeight  '",'
        '"' ipbf-ttLoadTag.palletWeight  '",'
        '"' fReplaceQuotes(IF AVAILABLE bf-itemfg THEN bf-itemfg.part-dscr1 ELSE "") '",'
        '"' fReplaceQuotes(IF AVAILABLE bf-itemfg THEN bf-itemfg.part-dscr2 ELSE "") '",'
        '"' fReplaceQuotes(IF AVAILABLE bf-itemfg THEN bf-itemfg.part-dscr3 ELSE "") '",'
        '"' fReplaceQuotes(ipbf-ttLoadTag.lotID) '",'
        '"' ipbf-ttLoadTag.palletID '",'
        '"' IF AVAILABLE bf-cust THEN bf-cust.spare-int-1 + 1 ELSE 0 '",'
        '"' iTagCounter '",'
        '"' ipbf-ttLoadTag.totalTags '",'
        '"' REPLACE(ipbf-ttLoadTag.shipNotes[1],'"', '') '",'
        '"' REPLACE(ipbf-ttLoadTag.shipNotes[2],'"', '') '",'
        '"' REPLACE(ipbf-ttLoadTag.shipNotes[3],'"', '') '",'
        '"' REPLACE(ipbf-ttLoadTag.shipNotes[4],'"', '') '",'
        '"' ipbf-loadtag.loc '",'
        '"' ipbf-loadtag.loc-bin '",'
        '"' ipbf-ttLoadTag.jobQuantity '",'
        '"' STRING(ipbf-ttLoadTag.runShip,"R&S/WHSE")  '",'
        '"' STRING(ipbf-loadtag.pallet-no)  '",'
        '"' STRING(ipbf-ttLoadTag.zone)  '",'
        '"' ipbf-loadtag.createUser '",'
        '"' STRING(ipbf-loadtag.tag-date,"99/99/9999") '",'
        '"' STRING(ipbf-loadtag.tag-time,"HH:MM AM") '",'
        '"' STRING(TODAY,"99/99/9999") '",'
        '"' STRING(TIME,"HH:MM AM") '"'.
        .
        
        IF glUpdateLoadTagSSCC THEN PUT UNFORMATTED ',"' ipbf-ttLoadTag.sscc '"'.  
        
        PUT SKIP.    
    END.
    
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE pPrintLoadTagHeader PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    OUTPUT TO VALUE (gcLabelMatrixLoadTagOutputPath + gcLabelMatrixLoadTagOutputFile) APPEND.
    
    PUT UNFORMATTED
        "CUSTOMER,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL,"
        "SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPCOUNTRY,SHIPZIP,"
        "SOLDCODE,SOLDNAME,SOLDADD1,SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP,"
        "INAME,DUEDATE,RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT,"
        "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,"
        "TAG#,PARTIAL,CASECODE,SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,"
        "DN5,DN6,DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".
    
    IF CAN-DO("ASI,SSLABEL",gcLoadTag) THEN
        PUT UNFORMATTED ",COUNTER#,RFIDTag".

    PUT UNFORMATTED 
        ",DUEDATEJOBLINE,DUEDATEJOB,LINE#,UnitWt,PalletWt,FGdesc1,FGdesc2,FGdesc3,FG Lot#,"
        "PalletCode,PalletID,TagCounter,TagCountTotal,"
        "RN1,RN2,RN3,RN4,WareHouse,Bin,JobQty,RunShip,Pallet type,Zone,CreatedBy,CreateDate,CreateTime,PrintDate,PrintTime".

    IF glUpdateLoadTagSSCC THEN 
        PUT UNFORMATTED ",SSCC".
    
    PUT SKIP.
    
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE pPrintView:
DEFINE INPUT PARAMETER ipcCasLabel AS CHARACTER .
DEFINE INPUT PARAMETER iplPrintView AS LOGICAL .

DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFax   AS CHARACTER NO-UNDO.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(85)}

    SESSION:SET-WAIT-STATE ("general").
   
    IF iplPrintView THEN DO:
        IF NOT lBussFormModle THEN
           PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORM "x(50)".
         ELSE
           PUT "<PREVIEW></PROGRESS>" FORM "x(50)".
    END.
    ELSE DO:
       PUT "<PRINTER?><FORMAT=LEGAL></PROGRESS>" FORM "x(50)".
    END.

        iCountPallet = 0 .
        FOR EACH tt-word-print NO-LOCK BREAK
                                BY tt-word-print.ord-no 
                                BY tt-word-print.i-no:
            FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ cocode
             AND loadtag.item-type EQ NO
                AND loadtag.tag-no EQ TRIM(tt-word-print.tag-no)
                USE-INDEX tag NO-ERROR.
            iCountPallet = iCountPallet + 1 .
                                
           IF ipcCasLabel EQ "loadtag.xpr" THEN DO:
               {oe/rep/lodxprntstd.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag1.xpr" THEN DO:
               {oe/rep/lodxprnt.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag2.xpr" THEN DO:
               {oe/rep/lodxprnt2.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag3.xpr" THEN DO:
               {oe/rep/lodxprnt3.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag4.xpr" THEN DO:
               {oe/rep/lodxprnt4.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag5.xpr" THEN DO:
               {oe/rep/lodxprnt5.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag6.xpr" THEN DO:
               {oe/rep/lodxprnt6.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag7.xpr" THEN DO:
               {oe/rep/lodxprnt7.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag8.xpr" THEN DO:
               {oe/rep/lodxprnt8.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag9.xpr" THEN DO:
               {oe/rep/lodxprnt9.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag10.xpr" THEN DO:
               {oe/rep/lodxprnt10.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag11.xpr" THEN DO:
               {oe/rep/lodxprnt11.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag12.xpr" THEN DO:
               {oe/rep/lodxprnt12.i} 
           END.
           ELSE IF ipcCasLabel EQ "loadtag13.xpr" THEN DO:
               {oe/rep/lodxprnt13.i}
           END.
           ELSE IF ipcCasLabel EQ "Logo.xpr" THEN DO:
               {oe/rep/lodxprntLogo.i} 
           END.
           ELSE IF ipcCasLabel EQ "NoLogo.xpr" THEN DO:
               {oe/rep/lodxprntNoLogo.i} 
           END.
    
         IF NOT LAST(tt-word-print.i-no) THEN PAGE .
        END.
   
    OUTPUT CLOSE.
    SESSION:SET-WAIT-STATE ("").

    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).
 
        
END PROCEDURE.

PROCEDURE pCreateTTLoadTagFromItem:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opTTLoadTagRecordID  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemfg    FOR itemfg.
    DEFINE BUFFER bf-style     FOR style.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-company   FOR company.
        
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

        FIND FIRST bf-company NO-LOCK
             WHERE bf-company.company EQ ipcCompany
             NO-ERROR.
        IF NOT AVAILABLE bf-company THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid company '" + ipcCompany + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ ipcCompany
               AND bf-itemfg.i-no    EQ ipcItemID
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid item # '" + ipcItemID + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        CREATE bf-ttLoadTag.
        ASSIGN
            bf-ttLoadTag.recordID      = fGetNextTTLoadTagRecordID()
            bf-ttLoadTag.company       = bf-itemfg.company
            bf-ttLoadTag.itemID        = bf-itemfg.i-no
            bf-ttLoadTag.custPartNo    = bf-itemfg.part-no
            bf-ttLoadTag.itemName      = bf-itemfg.i-name
            bf-ttLoadTag.upcNo         = bf-itemfg.upc-no
            bf-ttLoadTag.boxLen        = bf-itemfg.l-score[50]
            bf-ttLoadTag.boxWid        = bf-itemfg.w-score[50]
            bf-ttLoadTag.boxDep        = bf-itemfg.d-score[50]
            bf-ttLoadTag.style         = bf-itemfg.style
            bf-ttLoadTag.vendor        = bf-company.name
            bf-ttLoadTag.zoneID        = bf-itemfg.spare-char-4
            bf-ttLoadTag.tagStatus     = "Created"
            bf-ttLoadTag.recordSource  = "FGITEM"
            .

        IF bf-ttLoadTag.style NE "" THEN DO:
            FIND FIRST bf-style NO-LOCK
                 WHERE bf-style.company EQ ipcCompany
                   AND bf-style.style   EQ ttLoadTag.style
                 NO-ERROR.
            IF AVAILABLE bf-style THEN
                bf-ttLoadTag.styleDesc = bf-style.dscr.
        END.
        
        opTTLoadTagRecordID = bf-ttLoadTag.recordID.
    END.
END PROCEDURE.

PROCEDURE pUpdateConfig PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE oSSLoadTagJobConfig AS system.Config NO-UNDO.

    oSSLoadTagJobConfig = CAST(system.ConfigLoader:Instance:GetConfig("SSLoadTagJob"), system.Config).
    
    IF VALID-OBJECT(oSSLoadTagJobConfig) THEN
        ASSIGN
            glUpdateLoadTagSSCC                    = LOGICAL(oSSLoadTagJobConfig:GetAttributeValue("UpdateLoadTagSSCC", "Active"))
            glUpdateLocBinFromItemFG               = LOGICAL(oSSLoadTagJobConfig:GetAttributeValue("UpdateLocBinFromItemFG", "Active"))
            glUpdateLocBinFromFGBin                = LOGICAL(oSSLoadTagJobConfig:GetAttributeValue("UpdateLocBinFromFGBin", "Active"))
            glCreateFGReceipts                     = LOGICAL(oSSLoadTagJobConfig:GetAttributeValue("CreateFGReceipts", "Active"))
            glCheckClosedStatus                    = LOGICAL(oSSLoadTagJobConfig:GetAttributeValue("CheckClosedStatus", "Active"))
            glUpdateSetWithMaxQuantity             = LOGICAL(oSSLoadTagJobConfig:GetAttributeValue("UpdateSetWithMaxQuantity", "Active"))
            glCreateRFIDTag                        = LOGICAL(oSSLoadTagJobConfig:GetAttributeValue("CreateRFIDTag", "Active"))
            glCreateComponenetTagsForSetHeaderItem = LOGICAL(oSSLoadTagJobConfig:GetAttributeValue("CreateComponenetTagsForSetHeaderItem", "Active"))
            giFGSetRec                             = INTEGER(oSSLoadTagJobConfig:GetAttributeValue("FGSetRec", "Value"))
            gcLoadTag                              = STRING(oSSLoadTagJobConfig:GetAttributeValue("LoadTag", "Printer"))
            gcLabelMatrixLoadTagOutputFile         = STRING(oSSLoadTagJobConfig:GetAttributeValue("LabelMatrixLoadTagOutputFilePath", "File"))
            gcLabelMatrixLoadTagOutputPath         = STRING(oSSLoadTagJobConfig:GetAttributeValue("LabelMatrixLoadTagOutputFilePath", "Path"))
            NO-ERROR.
END PROCEDURE.


PROCEDURE pUpdateTTLoadTagEstimateDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-est       FOR est.
    DEFINE BUFFER bf-eb        FOR eb.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
                        
        FIND FIRST bf-est NO-LOCK
             WHERE bf-est.company EQ ipcCompany
               AND bf-est.est-no  EQ ipcEstimateID
             NO-ERROR.

        IF AVAILABLE bf-est THEN
            FIND FIRST bf-eb NO-LOCK
                 WHERE bf-eb.company   EQ bf-est.company
                   AND bf-eb.est-no    EQ bf-est.est-no
                   AND bf-eb.form-no   EQ bf-ttLoadTag.formNo
                   AND (bf-eb.blank-no EQ bf-ttLoadTag.blankNo OR bf-ttLoadTag.blankNo EQ 0)
                 NO-ERROR.

/*              Need to configure if the estimate information is for a PO       */
/*              FIND FIRST bf-eb NO-LOCK                                        */
/*                   WHERE bf-eb.company  EQ bf-ttLoadTag.company               */
/*                     AND bf-eb.est-no   EQ bf-ttLoadTag.estID                 */
/*                     AND bf-eb.stock-no EQ bf-ttLoadTag.itemID                */
/*                   NO-ERROR.                                                  */

        IF AVAILABLE bf-eb THEN
            ASSIGN
                bf-ttLoadTag.flute      = bf-eb.flute
                bf-ttLoadTag.test       = bf-eb.test
                bf-ttLoadTag.caseNo     = bf-eb.cas-no
                bf-ttLoadTag.palletID   = bf-eb.tr-no
                bf-ttLoadTag.partDscr2  = bf-eb.part-dscr2
/*              The following variable assignments need to be configured if user does not choose to enter quantity */   
/*              manually. Also need to configure if this assignment is for a component of a set                    */             
/*              bf-ttLoadTag.pcs        = bf-eb.cas-cnt                                                            */
/*              bf-ttLoadTag.bundle     = bf-eb.cas-pal                                                            */
/*              bf-ttLoadTag.totalUnit  = bf-ttLoadTag.pcs * bf-ttLoadTag.bundle                                   */
/*              bf-ttLoadTag.formNo     = bf-eb.form-no                                                            */
/*              bf-ttLoadTag.totalTags  = ((bf-ttLoadTag.orderQuantity / bf-ttLoadTag.totalUnit) + .49) +          */
/*                                         (IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1) .            */
                .

/*        IF AVAILABLE bf-eb AND bf-eb.est-type EQ 2 THEN                                                                           */
/*            bf-ttLoadTag.jobQuantity = bf-ttLoadTag.jobQuantity * (IF bf-eb.cust-% GT 0 THEN bf-eb.cust-% ELSE 1)  .              */
/*        ELSE IF AVAILABLE bf-eb AND bf-eb.est-type EQ 6  THEN                                                                     */
/*            bf-ttLoadTag.jobQuantity = bf-ttLoadTag.jobQuantity * (IF bf-eb.quantityPerSet GT 0 THEN bf-eb.quantityPerSet ELSE 1).*/
            
    END.    
END PROCEDURE.

PROCEDURE pUpdateTTLoadTagCustShipTo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-cust      FOR cust.
    DEFINE BUFFER bf-cust-part FOR cust-part.
    DEFINE BUFFER bf-shipto    FOR shipto.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

        FIND FIRST bf-cust NO-LOCK
             WHERE bf-cust.company EQ ipcCompany
               AND bf-cust.cust-no EQ ipcCustID
             NO-ERROR.
        IF NOT AVAILABLE bf-cust THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid customer # '" + ipcCustID + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
            
        ASSIGN
            bf-ttLoadTag.custID = bf-cust.cust-no
            bf-ttLoadTag.custName  = bf-cust.name
            .
            
        FOR EACH bf-cust-part NO-LOCK 
            WHERE bf-cust-part.company EQ ipcCompany   
              AND bf-cust-part.i-no    EQ ipcItemID 
              AND bf-cust-part.cust-no EQ ipcCustID
              AND bf-cust-part.part-no NE "":
            bf-ttLoadTag.custPartNo = bf-cust-part.part-no.
            LEAVE.
        END.

        RUN oe/custxship.p (
            INPUT  bf-cust.company,
            INPUT  bf-cust.cust-no,
            INPUT  bf-cust.cust-no,
            BUFFER bf-shipto
            ).
        IF AVAILABLE bf-shipto THEN
            ASSIGN
                bf-ttLoadTag.shipID       = bf-shipto.ship-id
                bf-ttLoadTag.shipName     = bf-shipto.ship-name
                bf-ttLoadTag.shipAddress1 = bf-shipto.ship-add[1]
                bf-ttLoadTag.shipAddress2 = bf-shipto.ship-add[2]
                bf-ttLoadTag.shipCity     = bf-shipto.ship-city
                bf-ttLoadTag.shipState    = bf-shipto.ship-state
                bf-ttLoadTag.shipCountry  = bf-shipto.country
                bf-ttLoadTag.shipZip      = bf-shipto.ship-zip
                bf-ttLoadTag.broker       = bf-shipto.broker
                .         
    END.    
END PROCEDURE.

PROCEDURE BuildLoadTagsFromJob:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/  
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobID2            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnits          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityPerPallet AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPartial           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPalletTags        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField1        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField2        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField3        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue1   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue2   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue3   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
  
    RUN pBuildLoadTagsFromJob (
        INPUT  ipcCompany,
        INPUT  ipcJobID,
        INPUT  ipiJobID2,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        INPUT  ipcItemID,
        INPUT  ipiQuantity,
        INPUT  ipiSubUnits,
        INPUT  ipiSubUnitsPerUnit,
        INPUT  ipiQuantityPerPallet,
        INPUT  ipiPartial,
        INPUT  ipiPalletTags,
        INPUT  ipiCopies,
        INPUT  ipcUserField1,
        INPUT  ipcUserField2,
        INPUT  ipcUserField3,
        INPUT  ipcUserFieldValue1,
        INPUT  ipcUserFieldValue2,
        INPUT  ipcUserFieldValue3,
        OUTPUT lError,
        OUTPUT cMessage
        ).
END PROCEDURE.

PROCEDURE pBuildLoadTagsFromJob PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobID2            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnits          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityPerPallet AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPartial           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPalletTags        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField1        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField2        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField3        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue1   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue2   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue3   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iTTLoadTagRecordID AS INTEGER NO-UNDO.
    DEFINE VARIABLE lSetsCreated       AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-job       FOR job.
    DEFINE BUFFER bf-job-hdr   FOR job-hdr.
    DEFINE BUFFER bf-oe-ord    FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl   FOR oe-ordl.
    DEFINE BUFFER bf-shipto    FOR shipto.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadtag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
                            
        FIND FIRST bf-job NO-LOCK
             WHERE bf-job.company EQ ipcCompany
               AND bf-job.job-no  EQ ipcJobID
               AND bf-job.job-no2 EQ ipiJobID2
             NO-ERROR.
        IF AVAILABLE bf-job THEN
            FIND FIRST bf-job-hdr NO-LOCK
                 WHERE bf-job-hdr.company  EQ ipcCompany
                   AND bf-job-hdr.job      EQ bf-job.job
                   AND bf-job-hdr.job-no   EQ ipcJobID
                   AND bf-job-hdr.job-no2  EQ ipiJobID2
                   AND bf-job-hdr.frm      EQ ipiFormNo
                   AND bf-job-hdr.blank-no EQ ipiBlankNo
                   AND bf-job-hdr.i-no     EQ ipcItemID
                   AND bf-job-hdr.opened   EQ TRUE
                 NO-ERROR.
        
        IF NOT AVAILABLE bf-job-hdr THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid Job # '" + ipcJobID + "-" + STRING(ipiJobID2,"99") + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        RUN pCreateTTLoadTagFromItem (
            INPUT  bf-job-hdr.company,
            INPUT  bf-job-hdr.i-no,
            OUTPUT iTTLoadTagRecordID,
            OUTPUT oplError,
            OUTPUT opcMessage
            ).
        IF oplError THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
            
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ iTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Error while populating loadtag record'"
                .            
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        RUN pUpdateTTLoadTagCustShipTo (
            INPUT  bf-job-hdr.company,
            INPUT  bf-job-hdr.i-no,
            INPUT  bf-job-hdr.cust-no,
            INPUT  bf-ttLoadTag.recordID,
            OUTPUT oplError,
            OUTPUT opcMessage
            ).
        IF oplError THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        
        ASSIGN
            bf-ttLoadTag.orderID       = bf-job-hdr.ord-no
            bf-ttLoadTag.jobID         = bf-job-hdr.job-no
            bf-ttLoadTag.jobID2        = bf-job-hdr.job-no2
            bf-ttLoadTag.itemID        = bf-job-hdr.i-no
            bf-ttLoadTag.overPct       = 0
            bf-ttLoadTag.qtyBefore     = ipiQuantity
            bf-ttLoadTag.ordQuantity   = bf-ttLoadTag.qtyBefore * (1 + (bf-ttLoadTag.overPct / 100))
            bf-ttLoadTag.dueDate       = bf-job.start-date
            bf-ttLoadTag.estID         = bf-job.est-no
            bf-ttLoadTag.formNo        = bf-job-hdr.frm
            bf-ttLoadTag.blankNo       = bf-job-hdr.blank-no
            bf-ttLoadTag.tareWeight    = 10
            bf-ttLoadTag.uom           = "EA"
            bf-ttLoadTag.mult          = ipiCopies
            bf-ttLoadTag.dueDateJob    = IF bf-job.due-date <> ? THEN STRING(bf-job.due-date, "99/99/9999") ELSE ""
            bf-ttLoadTag.dueDateJobHdr = IF bf-job-hdr.due-date <> ? THEN STRING(bf-job-hdr.due-date, "99/99/9999") ELSE ""
            bf-ttLoadTag.jobQuantity   = bf-job-hdr.qty
            bf-ttLoadTag.ipReturn      = NO
            bf-ttLoadTag.tagStatus     = "Created"
            bf-ttLoadTag.recordSource  = "JOB"
            .

        RUN pGetReleaseInfo (
            INPUT  ROWID(bf-job-hdr),
            OUTPUT bf-ttLoadTag.custPONo,
            OUTPUT bf-ttLoadTag.relDate,
            OUTPUT bf-ttLoadTag.rellotID,
            OUTPUT bf-ttLoadTag.shipNotes,
            OUTPUT bf-ttLoadTag.relQuantity,
            OUTPUT bf-ttLoadTag.orderDesc1,
            OUTPUT bf-ttLoadTag.orderDesc2,
            OUTPUT bf-ttLoadTag.runShip
            ).
            
        bf-ttLoadTag.lotID = bf-ttLoadTag.rellotID.

        RUN pUpdateTTLoadTagEstimateDetails (
            INPUT bf-job.company,
            INPUT bf-job.est-no,
            INPUT bf-ttLoadTag.recordID
            ).

        ASSIGN
            bf-ttLoadTag.jobQuantity = ipiQuantity
            bf-ttLoadTag.pcs         = ipiSubUnits
            bf-ttLoadTag.bundle      = ipiSubUnitsPerUnit
            bf-ttLoadTag.partial     = ipiPartial
            bf-ttLoadTag.totalUnit   = ipiQuantityPerPallet
            bf-ttLoadTag.totalTags   = ipiPalletTags
            .

        IF ipcUserField1 EQ "Lot Number" THEN
            bf-ttLoadTag.lotID = ipcUserFieldValue1.
        ELSE IF ipcUserField2 EQ "Lot Number" THEN
            bf-ttLoadTag.lotID = ipcUserFieldValue2.
        ELSE IF ipcUserField3 EQ "Lot Number" THEN
            bf-ttLoadTag.lotID = ipcUserFieldValue3.
                
        IF ipcUserField1 EQ "Weight" THEN
            bf-ttLoadTag.unitWeight = DECIMAL(ipcUserFieldValue1).
        ELSE IF ipcUserField2 EQ "Weight" THEN
            bf-ttLoadTag.unitWeight = DECIMAL(ipcUserFieldValue2).
        ELSE IF ipcUserField3 EQ "Weight" THEN
            bf-ttLoadTag.unitWeight = DECIMAL(ipcUserFieldValue3).

        IF ipcUserField1 EQ "CustPo" THEN
            bf-ttLoadTag.custPONo = ipcUserFieldValue1.
        ELSE IF ipcUserField2 EQ "CustPo" THEN
            bf-ttLoadTag.custPONo = ipcUserFieldValue2.
        ELSE IF ipcUserField3 EQ "CustPo" THEN
            bf-ttLoadTag.custPONo = ipcUserFieldValue3.

        IF ipcUserField1 EQ "Overs" THEN
            bf-ttLoadTag.overPct = DECIMAL(ipcUserFieldValue1).
        ELSE IF ipcUserField2 EQ "Overs" THEN
            bf-ttLoadTag.overPct = DECIMAL(ipcUserFieldValue2).
        ELSE IF ipcUserField3 EQ "Overs" THEN
            bf-ttLoadTag.overPct = DECIMAL(ipcUserFieldValue3).
                         
        IF bf-ttLoadTag.partial EQ ? THEN 
            bf-ttLoadTag.partial = 0. 
        
        IF glCreateComponenetTagsForSetHeaderItem THEN DO:
            RUN pCreateSetComponentsForTTLoadTagItem (
                INPUT  bf-ttLoadTag.recordID,
                OUTPUT lSetsCreated
                ).
    
            IF lSetsCreated THEN
                DELETE bf-ttLoadTag.
        END.
    END. 
END PROCEDURE.

PROCEDURE pGetReleaseInfo:
/*------------------------------------------------------------------------------
      Purpose: Given a job-hdr returns the release details
      Parameters:  <none>
      Notes: This is a replacement procedure for get-rel-info     
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriJobHdr    AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustPONO   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtRelDate   AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRelLotID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcShipNotes  AS CHARACTER NO-UNDO EXTENT 4.
    DEFINE OUTPUT PARAMETER opiRelQty     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcOrderDesc1 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcOrderDesc2 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRunShip    AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-rel  FOR oe-rel.
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    
    FIND FIRST bf-job-hdr NO-LOCK
         WHERE ROWID(bf-job-hdr) EQ ipriJobHdr
         NO-ERROR .
    IF NOT AVAILABLE bf-job-hdr THEN
        RETURN.
    
    IF bf-job-hdr.ord-no EQ 0 THEN
        RETURN.

    FIND FIRST bf-oe-ordl NO-LOCK
         WHERE bf-oe-ordl.company EQ bf-job-hdr.company
           AND bf-oe-ordl.ord-no  EQ bf-job-hdr.ord-no
           AND bf-oe-ordl.i-no    EQ bf-job-hdr.i-no
         NO-ERROR.
    
    IF NOT AVAILABLE bf-oe-ordl THEN
        RETURN.
    
    ASSIGN
        opcOrderDesc1 = bf-oe-ordl.part-dscr1
        opcOrderDesc2 = bf-oe-ordl.part-dscr2
        oplRunShip    = bf-oe-ordl.whsed
        .
        
    FOR EACH bf-oe-rell NO-LOCK
        WHERE bf-oe-rell.company  EQ bf-oe-ordl.company
          AND bf-oe-rell.ord-no   EQ bf-oe-ordl.ord-no
          AND bf-oe-rell.i-no     EQ bf-oe-ordl.i-no
          AND bf-oe-rell.line     EQ bf-oe-ordl.line,
        FIRST bf-oe-relh NO-LOCK
        WHERE bf-oe-relh.r-no     EQ bf-oe-rell.r-no
           BY bf-oe-relh.rel-date
           BY bf-oe-relh.r-no:

        ASSIGN
            opcCustPONo     = bf-oe-rell.po-no
            opdtRelDate     = bf-oe-relh.rel-date
            opcShipNotes[1] = bf-oe-relh.ship-i[1]
            opcShipNotes[2] = bf-oe-relh.ship-i[2]
            opcShipNotes[3] = bf-oe-relh.ship-i[3]
            opcShipNotes[4] = bf-oe-relh.ship-i[4]
            opiRelQty       = bf-oe-rell.qty 
            .
        LEAVE.
    END.

    IF AVAILABLE bf-oe-rell THEN
        FIND FIRST bf-oe-rel NO-LOCK
             WHERE bf-oe-rel.r-no EQ bf-oe-rell.link-no
             NO-ERROR.
    ELSE DO:
        FOR EACH bf-oe-rel NO-LOCK
            WHERE bf-oe-rel.company  EQ bf-oe-ordl.company
              AND bf-oe-rel.ord-no   EQ bf-oe-ordl.ord-no
              AND bf-oe-rel.i-no     EQ bf-oe-ordl.i-no
              AND bf-oe-rel.line     EQ bf-oe-ordl.line
              AND bf-oe-rel.rel-no   EQ 0
            BY bf-oe-rel.rel-date
            BY bf-oe-rel.r-no:
            ASSIGN
                opcCustPONo     = (IF bf-oe-rel.po-no GT "" THEN bf-oe-rel.po-no ELSE bf-oe-ordl.po-no)
                opdtRelDate     = bf-oe-rel.rel-date
                opcShipNotes[1] = bf-oe-rel.ship-i[1]
                opcShipNotes[2] = bf-oe-rel.ship-i[2]
                opcShipNotes[3] = bf-oe-rel.ship-i[3]
                opcShipNotes[4] = bf-oe-rel.ship-i[4]
                .
            LEAVE.
        END.
    END.  

    IF NOT AVAILABLE bf-oe-rel THEN DO:
        FOR EACH bf-oe-rel NO-LOCK
            WHERE bf-oe-rel.company  EQ bf-oe-ordl.company
              AND bf-oe-rel.ord-no   EQ bf-oe-ordl.ord-no
              AND bf-oe-rel.i-no     EQ bf-oe-ordl.i-no
              AND bf-oe-rel.line     EQ bf-oe-ordl.line
               BY bf-oe-rel.rel-date
               BY bf-oe-rel.r-no:

            ASSIGN 
                opcCustPONo     = (IF bf-oe-rel.po-no GT "" THEN bf-oe-rel.po-no ELSE bf-oe-ordl.po-no)
                opdtRelDate     = bf-oe-rel.rel-date
                opcShipNotes[1] = bf-oe-rel.ship-i[1]
                opcShipNotes[2] = bf-oe-rel.ship-i[2]
                opcShipNotes[3] = bf-oe-rel.ship-i[3]
                opcShipNotes[4] = bf-oe-rel.ship-i[4]
                .
            LEAVE.
        END.
    END.
    
    IF AVAILABLE bf-oe-rel THEN 
        ASSIGN opcRelLotID = bf-oe-rel.lot-no.

    opcCustPONo = bf-job-hdr.po-no.
END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION fGetNextTTLoadTagRecordID RETURNS INTEGER PRIVATE
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE iNextRecordID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    
    FIND LAST bf-ttLoadTag NO-LOCK NO-ERROR.
    IF AVAILABLE bf-ttLoadTag THEN
        iNextRecordID = bf-ttLoadTag.recordID.
        
    iNextRecordID = iNextRecordID + 1.
    
    RETURN iNextRecordID.
END FUNCTION.

FUNCTION fReplaceQuotes RETURNS CHARACTER PRIVATE
	(INPUT ipcField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    ipcField = REPLACE(ipcField, '"', "''").

    RETURN ipcField.
END FUNCTION.
