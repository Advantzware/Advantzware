
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

DEFINE BUFFER b-itemfg   FOR itemfg.
DEFINE BUFFER bf-eb      FOR eb.
DEFINE BUFFER bf-itemfg  FOR itemfg.
DEFINE BUFFER bf-est     FOR est.
DEFINE BUFFER bf-po-ordl FOR po-ordl.
DEFINE BUFFER bf-job-hdr FOR job-hdr.
DEFINE BUFFER bf-job     FOR job.
DEFINE BUFFER bf1-job-hdr FOR job-hdr.
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
        CREATE ttOnOrder.
        ASSIGN
            ttOnOrder.itemID      = po-ordl.i-no
            ttOnOrder.itemType    = po-ordl.item-type
            ttOnOrder.source        = "PO" /*Initial creation from PO, if created from Job then "Job"*/
            ttOnOrder.poID        = STRING(po-ordl.po-no)
            ttOnOrder.poLine      = po-ordl.line
            ttOnOrder.jobID       = po-ordl.job-no
            ttOnOrder.jobID2      = po-ordl.job-no2
            ttOnOrder.formNo      = po-ordl.s-num
            ttOnOrder.blankNo     = po-ordl.b-num
            ttOnOrder.quantityUOM = po-ordl.pr-qty-uom
            ttOnOrder.warehouseID = po-ord.loc. /*when creating from Job header job.loc*/
            
        IF po-ordl.pr-qty-uom = "EA" THEN     
            dConvQty = po-ordl.ord-qty.
        ELSE 
        DO:
            /*Convert the quantity as per Unit of Measure*/
            dConvQty = 0.
            RUN Conv_QuantityFromUOMtoUOM(itemfg.company, 
                                         itemfg.i-no, 
                                         "FG", 
                                         po-ordl.ord-qty,
                                         po-ordl.pr-qty-uom,
                                         "EA", 
                                         po-ordl.s-len, 
                                         po-ordl.s-wid, 
                                         0,
                                         po-ordl.ord-qty,
                                         0, 
                                         OUTPUT dConvQty,
                                         OUTPUT lError,
                                         OUTPUT cMessage).
                                         
            
        END.
        IF dConvQty - po-ordl.t-rec-qty GT 0 THEN    
            ttOnOrder.quantity = dConvQty - po-ordl.t-rec-qty.
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

            IF NOT(itemfg.isaset OR (AVAIL eb AND NOT eb.pur-man)) THEN
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
            CREATE ttOnOrder.
            ASSIGN
                ttOnOrder.itemID      = job-hdr.i-no
                //ttOnOrder.itemType    = po-ordl.item-type
                ttOnOrder.source        = "Job"  /*Initial creation from PO, if created from Job then "Job"*/
                ttOnOrder.poID        = job-hdr.po-no
                //ttOnOrder.poLine      = po-ordl.line
                ttOnOrder.jobID       = job-hdr.job-no
                ttOnOrder.jobID2      = job-hdr.job-no2
                ttOnOrder.formNo      = job-hdr.frm
                ttOnOrder.blankNo     = job-hdr.blank-no
                //ttOnOrder.quantityUOM = /*qunatity UOM field not available in job-hdr*/
                ttOnOrder.warehouseID = job-hdr.loc. /*when creating from Job header job.loc*/
            IF ld-qty LT job-hdr.qty THEN
                ttOnOrder.quantity = job-hdr.qty - ld-qty.
           // check if job-hdr.est-num in eb
            FIND FIRST eb WHERE eb.company = job-hdr.company 
                AND eb.est-no = job-hdr.est-no NO-LOCK NO-ERROR.
            IF AVAILABLE eb AND (eb.est-type = 2 OR eb.est-type = 6) THEN /*Set record*/
            DO:
                FOR EACH bf-eb NO-LOCK
                   WHERE bf-eb.company = job-hdr.company 
                     AND bf-eb.est-no = job-hdr.est-no:
                         /*set component not available in job-hdr - check the reftable in that case*/
                     IF NOT CAN-FIND(FIRST bf-job-hdr WHERE bf-job-hdr.company = bf-eb.company
                                                        AND bf-job-hdr.est-no = bf-eb.est-no
                                                        AND bf-job-hdr.frm = bf-eb.form-no) THEN
                     DO:
                         /*Used the existing logic to for reftable search*/
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
                             FIRST bf1-job-hdr NO-LOCK
                             WHERE bf1-job-hdr.company EQ bf-job.company
                             AND bf1-job-hdr.job     EQ bf-job.job
                             AND bf1-job-hdr.job-no  EQ bf-job.job-no
                             AND bf1-job-hdr.job-no2 EQ bf-job.job-no2
                             USE-INDEX job-no,
                             FIRST b-itemfg NO-LOCK
                             WHERE b-itemfg.company EQ job-hdr.company
                             AND b-itemfg.i-no    EQ job-hdr.i-no
                             AND b-itemfg.isaset  EQ YES:
                             CREATE ttOnOrder.
                             ASSIGN
                                 ttOnOrder.itemID      = job-hdr.i-no
                //ttOnOrder.itemType    = po-ordl.item-type
                                 ttOnOrder.source        = "Job"  /*Initial creation from PO, if created from Job then "Job"*/
                                 ttOnOrder.poID        = job-hdr.po-no
                //ttOnOrder.poLine      = po-ordl.line
                                 ttOnOrder.jobID       = job-hdr.job-no
                                 ttOnOrder.jobID2      = job-hdr.job-no2
                                 ttOnOrder.formNo      = job-hdr.frm
                                 ttOnOrder.blankNo     = job-hdr.blank-no
/*                                 ttOnOrder.quantity    = job-hdr.qty*/
                //ttOnOrder.quantityUOM = /*qunatity UOM field not available in job-hdr*/
                                 ttOnOrder.warehouseID = job-hdr.loc.
                                 
                             RUN fg/fullset.p (ROWID(b-itemfg)).
          
                             FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ reftable.code2:
                                 ld-qty = 0.
                                 RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                                     INPUT job-hdr.job-no,
                                     INPUT job-hdr.job-no2,
                                     INPUT job-hdr.i-no,
                                     INPUT NO,
                                     OUTPUT ld-qty).
                                 IF ld-qty LT job-hdr.qty * tt-fg-set.part-qty-dec THEN
                                     ttOnOrder.quantity = ttOnOrder.quantity + ((job-hdr.qty * tt-fg-set.part-qty-dec) - ld-qty).
              
                             END.
                         END. 
                     END. /*each reftable*/ 
                END. /*each bf-eb*/ 
            END. /*available eb Set record*/                
        END. /* FOR EACH job-hdr */
    END. /*available job-hdr*/    
    /*Get the quantity*/
    FOR EACH ttOnOrder:
        op-q-ono = op-q-ono + ttOnOrder.quantity.
    END.  
    IF op-q-ono LT 0 THEN op-q-ono = 0.
  
END. /*item*/