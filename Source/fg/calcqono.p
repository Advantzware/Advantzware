
/*------------------------------------------------------------------------
    File        : calcqonoNew.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Varun Agrawal
    Created     : Wed Dec 21 01:51:16 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ip-rowid AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER op-q-ono LIKE itemfg.q-ono NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEFINE VARIABLE deQty     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE loError   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chMessage AS CHARACTER NO-UNDO.

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

DEFINE BUFFER bf-set-itemfg  FOR itemfg.
DEFINE BUFFER bf-job         FOR job.
DEFINE BUFFER bf-set-job-hdr FOR job-hdr.
/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */
FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAILABLE itemfg THEN 
DO:
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company EQ itemfg.company
        AND po-ordl.i-no      EQ itemfg.i-no        
        AND po-ordl.item-type EQ NO
        AND po-ordl.opened    EQ YES
        AND po-ordl.stat      NE "C",
        FIRST po-ord NO-LOCK
        WHERE po-ord.company EQ itemfg.company 
        AND po-ord.po-no     EQ po-ordl.po-no:
            
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

            IF NOT(itemfg.isaset OR (AVAILABLE eb AND NOT eb.pur-man) OR (NOT AVAILABLE eb AND NOT itemfg.pur-man)) THEN
                NEXT.   
            
            /*record with job,form and blank already exist*/
            IF CAN-FIND(FIRST ttOnOrder WHERE ttOnOrder.jobID   = job-hdr.job-no
                AND ttOnOrder.jobID2  = job-hdr.job-no2
                AND ttOnOrder.formNo  = job-hdr.frm
                AND ttOnOrder.blankNo = job-hdr.blank-no) THEN 
                NEXT.
                
            deQty = 0.
            RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                INPUT job-hdr.job-no,
                INPUT job-hdr.job-no2,
                INPUT job-hdr.i-no,
                INPUT NO,
                OUTPUT deQty).
            
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
                INPUT deQty
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
        
            IF NOT(itemfg.isaset OR (AVAILABLE eb AND NOT eb.pur-man) OR (NOT AVAILABLE eb AND NOT itemfg.pur-man)) THEN
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
    DEFINE INPUT PARAMETER ipchItemID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iploItemType    AS LOGICAL   NO-UNDO.  
    DEFINE INPUT PARAMETER ipchSource      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipchPoID        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipinPoLine      AS INTEGER   NO-UNDO.  
    DEFINE INPUT PARAMETER ipchJobID       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipinJobID2      AS INTEGER   NO-UNDO.  
    DEFINE INPUT PARAMETER ipinFormNo      AS INTEGER   NO-UNDO.  
    DEFINE INPUT PARAMETER ipinBlankNo     AS INTEGER   NO-UNDO.  
    DEFINE INPUT PARAMETER ipchQuantityUOM AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipchWarehouseID AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipdeQty         AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipinSlen        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipinSWid        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipdeRecQty      AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE deJobQty            AS DECIMAL NO-UNDO.    
    DEFINE VARIABLE deQuantityPerSet    AS DECIMAL NO-UNDO.
    
    CREATE ttOnOrder.
    ASSIGN
        ttOnOrder.itemID      = ipchItemID
        ttOnOrder.itemType    = iploItemType
        ttOnOrder.source      = ipchSource /*Initial creation from PO, if created from Job then "Job"*/
        ttOnOrder.poID        = ipchPoID
        ttOnOrder.poLine      = ipinPoLine
        ttOnOrder.jobID       = ipchJobID
        ttOnOrder.jobID2      = ipinJobID2
        ttOnOrder.formNo      = ipinFormNo
        ttOnOrder.blankNo     = ipinBlankNo
        ttOnOrder.quantityUOM = ipchQuantityUOM
        ttOnOrder.warehouseID = ipchWarehouseID. /*when creating from Job header job.loc*/     
    
    IF ipchSource = "PO" THEN 
    DO: 
        IF ipchQuantityUOM = "EA" THEN     
            deJobQty = ipdeQty.                   
        ELSE 
        DO:
            /*Convert the quantity as per Unit of Measure*/
            deJobQty = 0.
            RUN Conv_QuantityFromUOMtoUOM(itemfg.company, 
                ipchItemID, 
                "FG", 
                ipdeQty,
                ipchQuantityUOM,
                "EA", 
                ipinSlen, 
                ipinSWid, 
                0,
                ipdeQty,
                0, 
                OUTPUT deJobQty,
                OUTPUT loError,
                OUTPUT chMessage).
        END.
        
        IF deJobQty - ipdeRecQty GT 0 THEN    
            ttOnOrder.quantity = deJobQty - ipdeRecQty.
    END. /*if PO*/
    IF ipchSource = "JOB" THEN 
    DO:
        deJobQty = 0.
        RUN fg/GetProductionQty.p (INPUT itemfg.company,
            INPUT ipchJobID,
            INPUT ipinJobID2,
            INPUT ipchItemID,
            INPUT NO,
            OUTPUT deJobQty).
        IF deJobQty LT ipdeQty THEN
            ttOnOrder.quantity = ipdeQty - deJobQty.
    END.
    IF ipchSource = "SetJob" THEN 
    DO: 
        FOR FIRST fg-set NO-LOCK
            WHERE fg-set.company EQ itemfg.company
            AND fg-set.part-no EQ itemfg.i-no
            AND fg-set.set-no  EQ ipchItemID:
                     
            ASSIGN 
                deQuantityPerSet = IF fg-set.qtyPerSet LT 0 THEN (1 / (fg-set.qtyPerSet * -1)) ELSE fg-set.qtyPerSet.
            
            RUN fg/GetProductionQty.p (INPUT itemfg.company,
                INPUT ipchJobID,
                INPUT ipinJobID2,
                INPUT ipchItemID,
                INPUT NO,
                OUTPUT deJobQty).
                
            IF deJobQty LT ipdeQty * deQuantityPerSet THEN
                ttOnOrder.quantity = ttOnOrder.quantity + ((ipdeQty * deQuantityPerSet) - deJobQty).
              
        END. /* FOR EACH tt-fg-set*/ 
    END.
END.
