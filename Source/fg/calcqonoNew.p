
/*------------------------------------------------------------------------
    File        : calcqonoNew.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Dec 21 01:51:16 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-q-ono LIKE itemfg.q-ono NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEFINE VARIABLE ld-qty     LIKE fg-bin.qty  NO-UNDO.
DEFINE VARIABLE v-set-item LIKE itemfg.i-no NO-UNDO.

DEFINE VARIABLE v-part-qty     AS INTEGER NO-UNDO.
DEFINE VARIABLE v-set-job-qty  AS INTEGER NO-UNDO.
DEFINE VARIABLE v-set-rcv-qty  AS INTEGER NO-UNDO.
DEFINE VARIABLE v-part-qty-dec AS DECIMAL NO-UNDO.
DEFINE VARIABLE dConvQty       AS DECIMAL NO-UNDO.
DEFINE VARIABLE lError         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

{fg/fullset.i NEW}
DEFINE TEMP-TABLE ttOnOrder
    FIELD itemID      AS CHARACTER 
    FIELD itemType    AS LOGICAL 
    FIELD source      AS CHARACTER  
    FIELD poID        AS CHARACTER 
    FIELD poLine      AS INTEGER
    FIELD jobID       AS CHARACTER
    FIELD jobID2      AS INTEGER
    FIELD formNo      AS INTEGER
    FIELD blankNo     AS INTEGER
    FIELD quantity    AS DECIMAL
    FIELD quantityUOM AS CHARACTER
    FIELD warehouseID AS CHARACTER.

DEFINE BUFFER bf-set-itemfg   FOR itemfg.
DEFINE BUFFER bf-eb      FOR eb.
DEFINE BUFFER bf-itemfg  FOR itemfg.
DEFINE BUFFER bf-est     FOR est.
DEFINE BUFFER bf-po-ordl FOR po-ordl.
DEFINE BUFFER bf-job-hdr FOR job-hdr.
DEFINE BUFFER bf-job     FOR job.
DEFINE BUFFER bf-set-job-hdr FOR job-hdr.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL itemfg THEN 
DO:
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company   EQ itemfg.company
        AND po-ordl.i-no      EQ itemfg.i-no
        /*        AND po-ordl.job-no    EQ ""*/
        AND po-ordl.item-type EQ NO
        AND po-ordl.opened    EQ YES
        AND po-ordl.stat      NE "C",
        FIRST po-ord WHERE
        po-ord.company EQ itemfg.company AND
        po-ord.po-no   EQ po-ordl.po-no
        NO-LOCK:
            
        IF po-ordl.job-no <> "" AND NOT CAN-FIND(FIRST job WHERE job.company EQ po-ordl.company 
            AND job.job-no  EQ po-ordl.job-no) THEN 
            NEXT.
                
        /*craete temp table data*/
        RUN pCreatettOnOrder(INPUT po-ordl.i-no,
        INPUT po-ordl.item-type,
        INPUT "PO",
        INPUT STRING(po-ordl.po-no),
        INPUT po-ordl.line,
        INPUT po-ordl.job-no,
        INPUT po-ordl.job-no2,
        INPUT po-ordl.s-num,
        INPUT po-ordl.b-num,
        INPUT po-ordl.pr-qty-uom,
        INPUT po-ord.loc,
        INPUT po-ordl.ord-qty,
        INPUT po-ordl.s-len,
        INPUT po-ordl.s-wid,
        INPUT po-ordl.t-rec-qty
        ).
    END. /*each po-ordl*/  
    
    /*check for availability of job-hdr*/  
    FIND FIRST job-hdr WHERE job-hdr.company EQ itemfg.company 
        AND job-hdr.i-no    EQ itemfg.i-no 
        AND job-hdr.opened  EQ YES NO-LOCK NO-ERROR.
            
    IF AVAILABLE job-hdr THEN 
    DO:
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ itemfg.company 
            AND job-hdr.i-no    EQ itemfg.i-no 
            AND job-hdr.opened  EQ YES 
            AND CAN-FIND(FIRST job 
            WHERE job.company EQ job-hdr.company 
            AND job.job     EQ job-hdr.job 
            AND job.job-no  EQ job-hdr.job-no 
            AND job.job-no2 EQ job-hdr.job-no2)
            USE-INDEX i-no:
                
            IF NOT itemfg.isaset THEN           
                FIND FIRST eb WHERE 
                    eb.company  EQ job-hdr.company AND
                    eb.est-no   EQ job-hdr.est-no AND
                    eb.form-no  EQ job-hdr.frm AND
                    eb.blank-no EQ job-hdr.blank-no
                    NO-LOCK NO-ERROR.

            IF NOT(itemfg.isaset OR (AVAIL eb AND NOT eb.pur-man) OR (NOT AVAIL eb AND NOT itemfg.pur-man)) THEN
                NEXT.   
            
            /*record with job,form and blank already exist*/
            IF CAN-FIND(FIRST ttOnOrder WHERE ttOnOrder.jobID   = job-hdr.job-no
                AND ttOnOrder.jobID2  = job-hdr.job-no2
                AND ttOnOrder.formNo  = job-hdr.frm
                AND ttOnOrder.blankNo = job-hdr.blank-no) THEN 
                NEXT.
                
            ld-qty = 0.
            RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                INPUT job-hdr.job-no,
                INPUT job-hdr.job-no2,
                INPUT job-hdr.i-no,
                INPUT NO,
                OUTPUT ld-qty).
            
            /*create temp-table*/ 
            RUN pCreatettOnOrder(INPUT job-hdr.i-no,
                NO,
                INPUT "Job",
                INPUT job-hdr.po-no,
                0,
                INPUT job-hdr.job-no,
                INPUT job-hdr.job-no2,
                INPUT job-hdr.frm,
                INPUT job-hdr.blank-no,
                INPUT "EA",
                INPUT job-hdr.loc,
                INPUT job-hdr.qty,
                INPUT 0,
                INPUT 0,
                INPUT ld-qty
                ).              
        END. /* FOR EACH job-hdr */
    END. /*available job-hdr*/
    ELSE
    FOR EACH reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ itemfg.company
        AND reftable.loc      EQ ""
        AND reftable.code2    EQ itemfg.i-no
        USE-INDEX code2,
        FIRST bf-job NO-LOCK /*each removed as company and job is part of unique index and the combination will have only one record*/
        WHERE bf-job.company EQ itemfg.company
        AND bf-job.job EQ INTEGER(reftable.CODE)
        AND bf-job.opened  EQ YES 
        USE-INDEX job,
        FIRST bf-set-job-hdr NO-LOCK
        WHERE bf-set-job-hdr.company EQ bf-job.company
        AND bf-set-job-hdr.job     EQ bf-job.job
        AND bf-set-job-hdr.job-no  EQ bf-job.job-no
        AND bf-set-job-hdr.job-no2 EQ bf-job.job-no2        
        USE-INDEX job-no,
        FIRST bf-set-itemfg NO-LOCK
        WHERE bf-set-itemfg.company EQ bf-set-job-hdr.company
        AND bf-set-itemfg.i-no    EQ bf-set-job-hdr.i-no
        AND bf-set-itemfg.isaset  EQ YES:
            
        FIND FIRST eb 
            WHERE eb.company EQ bf-set-job-hdr.company
            AND eb.est-no    EQ bf-set-job-hdr.est-no 
            AND eb.stock-no  EQ itemfg.i-no
            NO-LOCK NO-ERROR.
        
        IF NOT(itemfg.isaset OR (AVAIL eb AND NOT eb.pur-man) OR (NOT AVAIL eb AND NOT itemfg.pur-man)) THEN
            NEXT.
                    
        IF CAN-FIND(FIRST ttOnOrder WHERE ttOnOrder.jobID   = bf-set-job-hdr.job-no
            AND ttOnOrder.jobID2  = bf-set-job-hdr.job-no2
            AND ttOnOrder.formNo  = bf-set-job-hdr.frm
            AND ttOnOrder.blankNo = bf-set-job-hdr.blank-no) THEN 
            NEXT.
            
        RUN fg/fullset.p (ROWID(bf-set-itemfg)).               
                           
        /*create temp-table*/ 
        RUN pCreatettOnOrder(INPUT bf-set-job-hdr.i-no,
            NO,
            INPUT "SetJob",
            INPUT bf-set-job-hdr.po-no,
            0,
            INPUT bf-set-job-hdr.job-no,
            INPUT bf-set-job-hdr.job-no2,
            INPUT bf-set-job-hdr.frm,
            INPUT bf-set-job-hdr.blank-no,
            INPUT "EA",
            INPUT bf-set-job-hdr.loc,
            INPUT bf-set-job-hdr.qty,
            INPUT 0,
            INPUT 0,
            INPUT 0
            ).
    END. /*reftable*/

    /*Get the quantity*/
    FOR EACH ttOnOrder:
        op-q-ono = op-q-ono + ttOnOrder.quantity.
    END.  
    IF op-q-ono LT 0 THEN op-q-ono = 0.
        
  
END. /*item*/

PROCEDURE pCreatettOnOrder PRIVATE:
    DEFINE INPUT PARAMETER cItemID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER lItemType    AS LOGICAL   NO-UNDO.  
    DEFINE INPUT PARAMETER cSource      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cPoID        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iPoLine      AS INTEGER   NO-UNDO.  
    DEFINE INPUT PARAMETER cJobID       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER iJobID2      AS INTEGER   NO-UNDO.  
    DEFINE INPUT PARAMETER iFormNo      AS INTEGER   NO-UNDO.  
    DEFINE INPUT PARAMETER iBlankNo     AS INTEGER   NO-UNDO.  
    DEFINE INPUT PARAMETER cQuantityUOM AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER cWarehouseID AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER dQty         AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iSlen         AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iSWid         AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER drec-qty    AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dJobQty          AS DECIMAL NO-UNDO.    
    DEFINE VARIABLE deQuantityPerSet AS DECIMAL NO-UNDO.
    
    CREATE ttOnOrder.
    ASSIGN
        ttOnOrder.itemID      = cItemID
        ttOnOrder.itemType    = lItemType
        ttOnOrder.source      = cSource /*Initial creation from PO, if created from Job then "Job"*/
        ttOnOrder.poID        = cPoID
        ttOnOrder.poLine      = iPoLine
        ttOnOrder.jobID       = cJobID
        ttOnOrder.jobID2      = iJobID2
        ttOnOrder.formNo      = iFormNo
        ttOnOrder.blankNo     = iBlankNo
        ttOnOrder.quantityUOM = cQuantityUOM
        ttOnOrder.warehouseID = cWarehouseID. /*when creating from Job header job.loc*/     
    
    IF cSource = "PO" THEN 
    DO: 
        IF cQuantityUOM = "EA" THEN     
            dJobQty = dQty.                   
        ELSE 
        DO:
            /*Convert the quantity as per Unit of Measure*/
            dJobQty = 0.
            RUN Conv_QuantityFromUOMtoUOM(itemfg.company, 
                cItemID, 
                "FG", 
                dQty,
                cQuantityUOM,
                "EA", 
                iSlen, 
                iSWid, 
                0,
                dQty,
                0, 
                OUTPUT dJobQty,
                OUTPUT lError,
                OUTPUT cMessage).
        END.
        
        IF dJobQty - drec-qty GT 0 THEN    
            ttOnOrder.quantity = dJobQty - drec-qty.
    END. /*if PO*/
    IF cSource = "JOB" THEN 
    DO:
        dJobQty = 0.
        RUN fg/GetProductionQty.p (INPUT itemfg.company,
            INPUT cJobID,
            INPUT iJobID2,
            INPUT cItemID,
            INPUT NO,
            OUTPUT dJobQty).
        IF dJobQty LT dqty THEN
            ttOnOrder.quantity = dqty - dJobQty.
    END.
    IF cSource = "SetJob" THEN 
    DO: 
        FOR FIRST fg-set NO-LOCK
            WHERE fg-set.company EQ itemfg.company
              AND fg-set.part-no EQ itemfg.i-no
              AND fg-set.set-no  EQ cItemID:
                     
            ASSIGN 
                deQuantityPerSet = IF fg-set.qtyPerSet LT 0 THEN (1 / (fg-set.qtyPerSet * -1)) ELSE fg-set.qtyPerSet.
            
            RUN fg/GetProductionQty.p (INPUT itemfg.company,
                INPUT cJobID,
                INPUT iJobID2,
                INPUT cItemID,
                INPUT NO,
                OUTPUT dJobQty).
                
            IF dJobQty LT dqty * deQuantityPerSet THEN
                ttOnOrder.quantity = ttOnOrder.quantity + ((dqty * deQuantityPerSet) - dJobQty).
              
        END. /* FOR EACH tt-fg-set*/ 
    END.
END.
