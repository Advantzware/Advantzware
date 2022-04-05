
/*------------------------------------------------------------------------
    File        : api/SendJob.p
    Purpose     : Returns the request data for a Job (job-hdr, job-mch, 
                  job-mat, job-prep)
    Syntax      :

    Description : Returns the request data for a job(job-hdr, job-mch, 
                  job-mat, job-prep)

    Author(s)   : Rahul Rawat
    Created     : Wed Mar 27 04:03:21 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    /* Variables to job's request data */
    DEFINE VARIABLE lcJobsData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatJobsData AS LONGCHAR  NO-UNDO.

    /* Variables to store job material request data */
    DEFINE VARIABLE lcJobHeaderData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatJobHeaderData AS LONGCHAR  NO-UNDO.

    /* Variables to store job material request data */
    DEFINE VARIABLE lcJobMatData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatJobMatData AS LONGCHAR  NO-UNDO.
    
    /* Variables to store job machine request data */
    DEFINE VARIABLE lcJobMachineData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatJobMachineData AS LONGCHAR  NO-UNDO.
    
     /* Variables to store job Preperation request data */
    DEFINE VARIABLE lcJobPrepData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatJobPrepData AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEstimate           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCSRID              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEnteredBy          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobDueDate         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobDueTime         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobStartDate       AS CHARACTER NO-UNDO.
    
    /* Job Header Variables*/
    
    DEFINE VARIABLE cOrder                        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartDate                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantity                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAverageCost                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLastPurchaseCost             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStandardItemCost             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStandardMaterialCost         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStandardLaborCost            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStandardFixedOverheadCost    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStandardVariableOverHeadCost AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItem                         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFGItemName                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomer                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cForm                         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlank                        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDueDate                      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerPO                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDueTime                      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreezeNotes                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSplitShip                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSquareInchPct                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreezeNotesDate              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWarehoused                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOpened                       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrinted                      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOReceivedDate               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderQty                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMfgDate                      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBoardCode                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGrain                        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCylinder                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTray                         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPriority                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIsPriority                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKeyItem                      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNumberOn                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUnitPrice                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPriceUom                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExtPrice                     AS CHARACTER NO-UNDO.
    
    /*Job Material variables*/
    
    DEFINE VARIABLE cItemNumber              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobMatItemStandardCost  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStandardCostUOM         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostPerUOM              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQtyToOrder              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemCost                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLength                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWidth                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBasisWeight             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLineNumber              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRMItem                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobMatBlank             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobMatForm              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAllocated               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityAllocated       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityIssued          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMRQuantity              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWasteQuantity           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDepth                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPONumber                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCrossGrain              AS CHARACTER NO-UNDO.
    
    /*Job Machine variables*/
    
    DEFINE VARIABLE cAnchored                   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMachineCode                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRWaste                    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRRate                     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMchMRvariableOverheadRate  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMchMRFixedOverheadRate     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRFixedOverheadCost        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDepartment                 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cItemName                   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cJobMchForm                 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cJobMchBlank                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cJobMchItem                 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cInteger                    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunHours                   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunMinutes                 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunSpeed                   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRHours                    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRMinutes                  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cJobMchLineNumber           AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunQuantity                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunStartDate               AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunStartTime               AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cQueueTime                  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cLagTime                    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cEndDate                    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cEndTime                    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSetupStartDate             AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSetupStartTime             AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSetupEndDate               AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSetupEndTime               AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRComplete                 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunComplete                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cJobMchDueDate              AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cJobMchDueTime              AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRTotalRate                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFixedOverheadRate          AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunProfitPct               AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRProfitPct                AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunRate                    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunTotalRate               AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cVariableOverheadRate       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRContributionRate         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRunContributionRate        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cJobMachineID               AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cJobMchWastePct             AS CHARACTER  NO-UNDO.
    
    /* Job Preps Variables */
    
    DEFINE VARIABLE cPrepCode         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobPrepQuantity  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemStandardCost AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStandardUOM      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostPerM         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrepDate         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrepTime         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobPrepPosted    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobPrepForm      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobPrepBlank     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSIMON            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMatOrLab         AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundDetail FOR APIOutboundDetail.
    DEFINE BUFFER bf-job-mat           FOR job-mat.
    DEFINE BUFFER bf-itemfg            FOR itemfg.             
/**********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
    
    IF ipcRequestHandler NE "" THEN
         RUN VALUE(ipcRequestHandler) (
             INPUT TABLE ttArgs,
             INPUT ipiAPIOutboundID,
             INPUT ipiAPIOutboundTriggerID,
             INPUT-OUTPUT ioplcRequestData,
             OUTPUT oplSuccess,
             OUTPUT opcMessage
             ). 
    ELSE DO:
        FIND FIRST APIOutboundDetail NO-LOCK
         WHERE APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND APIOutboundDetail.detailID      EQ "JobDetails"
           AND APIOutboundDetail.parentID      EQ "SendJob"
         NO-ERROR.
        
        IF NOT AVAILABLE APIOutboundDetail THEN DO:
            ASSIGN
                opcMessage = "No APIOutboundDetail record found for [ JobDetails ]"
                oplSuccess = FALSE
                .
            RETURN.
        END. 
            
        FIND FIRST ttArgs
             WHERE ttArgs.argType  EQ "ROWID"
               AND ttArgs.argKey   EQ "job"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid job record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST job NO-LOCK
             WHERE ROWID(job) EQ TO-ROWID(ttArgs.argValue)
             NO-ERROR.
        IF NOT AVAILABLE job THEN DO:
            ASSIGN
                opcMessage = "Invalid job ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        ASSIGN
            cCompany      = job.company  
            cJobNo        = job.job-no
            cLocation     = job.loc    
            cEstimate     = TRIM(job.est-no)            
            cJobNo2       = STRING(job.job-no2)            
            cIsPriority   = STRING(job.priority EQ 1,"true/false")
            cPriority     = STRING(job.priority)
            cCSRID        = job.csrUser_id
            cEnteredBy    = job.user-id
            cJobDueDate   = STRING(job.due-date)
            cJobDueTime   = STRING(job.due-time)
            cJobStartDate = STRING(job.start-date)
            .
    
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ "JobHeader"
           AND bf-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
         NO-ERROR.
         
    IF AVAILABLE bf-APIOutboundDetail THEN DO:       
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
              AND job-hdr.job-no  EQ job.job-no
              AND job-hdr.job-no2 EQ job.job-no2: 
                   
            FIND FIRST oe-ord NO-LOCK 
                 WHERE oe-ord.company EQ job-hdr.company
                   AND oe-ord.ord-no  EQ job-hdr.ord-no 
                 NO-ERROR.
            cPOReceivedDate = IF AVAILABLE oe-ord THEN STRING(oe-ord.poReceivedDate) ELSE "".  
            
            cFGItemName = "".
            
            FIND FIRST bf-itemfg NO-LOCK
                 WHERE bf-itemfg.company EQ job-hdr.company 
                   AND bf-itemfg.i-no    EQ job-hdr.i-no
                 NO-ERROR.
            IF AVAILABLE bf-itemfg THEN
                cFGItemName = bf-itemfg.i-name.
                     
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ job-hdr.company
                   AND oe-ordl.ord-no  EQ job-hdr.ord-no
                   AND oe-ordl.job-no  EQ job-hdr.job-no
                   AND oe-ordl.job-no2 EQ job-hdr.job-no2
                   AND oe-ordl.i-no    EQ job-hdr.i-no
                 NO-ERROR.  
            ASSIGN 
                cOrderQty = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.qty) ELSE "0"
                cMfgDate  = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.prom-date) ELSE ""
                cUnitPrice = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.price) ELSE "0"
                cPriceUom = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.pr-uom) ELSE "0"
                cExtPrice = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.t-price) ELSE "0"
                .                                            
                ASSIGN 
                    lcJobHeaderData               = bf-APIOutboundDetail.data
                    cOrder                        = STRING(job-hdr.ord-no)                                     
                    cStartDate                    = STRING(job-hdr.start-date)
                    cQuantity                     = STRING(job-hdr.qty)                    cAverageCost                  = STRING(job-hdr.avg-cost)                    cLastPurchaseCost             = STRING(job-hdr.last-cost)                    cStandardItemCost             = STRING(job-hdr.std-tot-cost)                    cStandardMaterialCost         = STRING(job-hdr.std-mat-cost)                    cStandardLaborCost            = STRING(job-hdr.std-lab-cost)                    cStandardFixedOverheadCost    = STRING(job-hdr.std-fix-cost)                    cStandardVariableOverHeadCost = STRING(job-hdr.std-var-cost)                    cItem                         = job-hdr.i-no                    cCustomer                     = job-hdr.cust-no                    cForm                         = STRING(job-hdr.frm)                    cBlank                        = STRING(job-hdr.blank-no)                    cDueDate                      = STRING(job-hdr.due-date)                    cCustomerPO                   = job-hdr.po-no                    cDueTime                      = STRING(job-hdr.due-time)                    cFreezeNotes                  = STRING(job-hdr.freezeNote)                    cSplitShip                    = STRING(job-hdr.splitShip)
                    cSquareInchPct                = STRING(job-hdr.sq-in)                    cFreezeNotesDate              = STRING(job-hdr.freezeNotesDate)                    cWarehoused                   = STRING(job-hdr.whsed)
                    cOpened                       = STRING(job-hdr.opened)
                    cPrinted                      = STRING(job-hdr.ftick-prnt)
                    cKeyItem                      = STRING(INTEGER(job-hdr.keyItem))
                    cNumberOn                     = STRING(job-hdr.n-on)
                    .
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "AverageCost",cAverageCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Blank",cBlank).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Customer",cCustomer). 
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "DueDate",cDueDate). 
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "DueTime",cDueTime). 
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "FreezeNotes",cFreezeNotes).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "FreezeNotesDate",cFreezeNotesDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Form",cForm).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Item",cItem).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "FGItemName",cFGItemName).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "LastPurchaseCost",cLastPurchaseCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Order",cOrder).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "CustomerPO",cCustomerPO).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Quantity",cQuantity).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "SplitShip",cSplitShip).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "StartDate",cStartDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "FixedOverHeadCost",cStandardFixedOverheadCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "StandardLaborCost",cStandardLaborCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "StandardMaterialCost",cStandardMaterialCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "StandardItemCost",cStandardItemCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "StandardVariableOverHeadCost",cStandardVariableOverHeadCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Warehoused",cWarehoused).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Opened",cWarehoused).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Printed",cPrinted).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "SquareInchPercentage",cSquareInchPct).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "POReceivedDate",cPOReceivedDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "MfgDate",cMfgDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "OrderQty",cOrderQty). 
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "KeyItem",cKeyItem). 
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "NumberOn",cNumberOn).                
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "UnitSellPrice",cUnitPrice).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "SellPriceUOM",cPriceUom).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "ExtendedSellingPrice",cExtPrice).
                lcConcatJobHeaderData = lcConcatJobHeaderData + lcJobHeaderData.
            END.
        END. 
        
        FIND FIRST bf-APIOutboundDetail NO-LOCK
             WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND bf-APIOutboundDetail.detailID      EQ "JobMaterial"
               AND bf-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
             NO-ERROR. 
        IF AVAILABLE bf-APIOutboundDetail THEN DO:  
            FOR EACH job-mat NO-LOCK
                WHERE job-mat.company EQ job.company
                  AND job-mat.job-no  EQ job.job-no
                  AND job-mat.job-no2 EQ job.job-no2:
                
                ASSIGN 
                    cBoardCode = ""
                    cCylinder  = ""
                    cTray      = ""
                    cGrain     = ""
                    .
                        
                FOR EACH job-hdr 
                    WHERE job-hdr.company EQ job-mat.company
                      AND job-hdr.job-no  EQ job-mat.job-no
                      AND job-hdr.job-no2 EQ job-mat.job-no2
                      AND job-hdr.frm     EQ job-mat.frm,
                      EACH ef
                      WHERE ef.company EQ job-hdr.company
                       AND ef.est-no   EQ job-hdr.est-no
                       AND ef.form-no  EQ job-hdr.frm,
                       EACH eb NO-LOCK
                        WHERE eb.company    EQ ef.company
                          AND eb.est-no     EQ ef.est-no
                          AND eb.form-no    EQ ef.form-no
                        BREAK BY ef.est-no
                              BY ef.form-no
                              BY eb.form-no 
                              BY eb.blank-no:
                        IF LAST-OF(eb.form-no) THEN DO:
                            FOR EACH bf-job-mat NO-LOCK
                                WHERE bf-job-mat.company  EQ job-hdr.company
                                  AND bf-job-mat.job      EQ job-hdr.job
                                  AND bf-job-mat.frm      EQ ef.form-no,
                                FIRST ITEM NO-LOCK 
                                WHERE item.company EQ job-hdr.company
                                  AND item.i-no    EQ bf-job-mat.i-no
                                  AND INDEX("BPR",item.mat-type) GT 0  
                                :
                                ASSIGN 
                                    cBoardCode = TRIM(item.i-name)
                                    cGrain     = TRIM(ef.xgrain)
                                    cCylinder  = TRIM(STRING(ef.gsh-len)) 
                                    .       
                            END. /* End of for each job-mat */ 
                            cTray = TRIM(eb.layer-pad).      
                        END. /* End of last-of(eb.form-no)*/ 
                    END.  /* End of for each eb */
                ASSIGN
                    lcJobMatData               = bf-APIOutboundDetail.data
                    cItemNumber                = job-mat.i-no
                    cJobMatItemStandardCost    = STRING(job-mat.std-cost)
                    cStandardCostUOM           = job-mat.sc-uom
                    cCostPerUOM                = STRING(job-mat.cost-m)
                    cQtyToOrder                = STRING(job-mat.qty)
                    cQuantityUOM               = job-mat.qty-uom
                    cLength                    = STRING(job-mat.len)
                    cWidth                     = STRING(job-mat.wid)
                    cBasisWeight               = STRING(job-mat.basis-w)
                    cLineNumber                = STRING(job-mat.line)
                    cRMItem                    = job-mat.rm-i-no
                    cJobMatBlank               = STRING(job-mat.blank-no)
                    cJobMatForm                = STRING(job-mat.frm)
                    cAllocated                 = STRING(all-flg)
                    cQuantityAllocated         = STRING(job-mat.qty-all)
                    cQuantityIssued            = STRING(job-mat.qty-iss)
                    cMRQuantity                = STRING(job-mat.qty-mr)
                    cWasteQuantity             = STRING(job-mat.qty-wst)
                    cDepth                     = STRING(job-mat.dep)
                    cPONumber                  = STRING(job-mat.po-no)
                    cCrossGrain                = IF job-mat.xGrain = "N" THEN "NO" ELSE IF job-mat.xGrain = "S" THEN "(S)heet"
                                                    ELSE IF job-mat.xGrain = "B" THEN "(B)lank" ELSE job-mat.xgrain
                    .
                 
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Allocated", cAllocated).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "BasisWeight", cBasisWeight).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Blank", cJobMatBlank).     
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "CostPerM", cCostPerUOM).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Depth", cDepth).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Form",cJobMatForm ).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Item",cItemNumber).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Length",cLength).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Line",cLineNumber).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "PoNumber",cPONumber).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "TotMRP",cQtyToOrder).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "QuantityAllocated",cQuantityAllocated).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "QuantityIssued",cQuantityIssued).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "MRQuantity",cMRQuantity).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "QuantityUOM",cQuantityUOM).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "WasteQuantity",cWasteQuantity).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "StandardCostUOM",cStandardCostUOM).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "ItemStandardCost",cJobMatItemStandardCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Width",cWidth).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "RawMaterialItem",cRMItem).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "CrossGrain",cCrossGrain).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "BoardCode",cBoardCode).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Grain",cGrain).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Cylinder",cCylinder).
                RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "Tray",cTray).
                
                lcConcatJobMatData  = lcConcatJobMatData  + "~n" + lcJobMatData.                                              
            END.
        END.
        FIND FIRST bf-APIOutboundDetail NO-LOCK
             WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND bf-APIOutboundDetail.detailID      EQ "JobMachine"
               AND bf-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
             NO-ERROR.
        IF AVAILABLE bf-APIOutboundDetail THEN DO:    
           FOR EACH job-mch NO-LOCK
               WHERE job-mch.company EQ job.company
                 AND job-mch.job-no  EQ job.job-no
                 AND job-mch.job-no2 EQ job.job-no2:
                     
               ASSIGN
                   lcJobMachineData           = bf-APIOutboundDetail.data
                   cAnchored                  = STRING(job-mch.anchored)
                   cMachineCode               = job-mch.m-code
                   cMRWaste                   = STRING(job-mch.mr-waste)
                   cMRRate                    = STRING(job-mch.mr-rate)
                   cMchMRvariableOverHeadRate = STRING(job-mch.mr-varoh)
                   cMchMRFixedOverheadRate    = STRING(job-mch.mr-fixoh)
                   cDepartment                = job-mch.dept
                   cItemName                  = job-mch.i-name
                   cJobMchItem                = job-mch.i-no
                   cJobMatForm                = STRING(job-mch.frm)
                   cRunHours                  = STRING(job-mch.run-hr)
                   cRunMinutes                = STRING(ROUND(job-mch.run-hr * 60, 0))
                   cRunSpeed                  = STRING(job-mch.speed)
                   cMRHours                   = STRING(job-mch.mr-hr)
                   cMRMinutes                 = STRING(ROUND(job-mch.mr-hr * 60, 0))
                   cJobMchBlank               = STRING(job-mch.blank-no)
                   cJobMchLineNumber          = STRING(job-mch.line)
                   cRunQuantity               = STRING(job-mch.run-qty)
                   cRunStartDate              = STRING(job-mch.start-date)
                   cRunStartTime              = STRING(job-mch.start-time)
                   cQueueTime                 = STRING(job-mch.queue-time)
                   cLagTime                   = STRING(job-mch.lag-time)
                   cEndDate                   = STRING(job-mch.end-date)
                   cEndTime                   = STRING(job-mch.end-time)
                   cSetupStartDate            = STRING(job-mch.start-date-su)
                   cSetupStartTime            = STRING(job-mch.start-time-su)
                   cSetupEndDate              = STRING(job-mch.end-date-su)
                   cSetupEndTime              = STRING(job-mch.end-time-su)
                   cMRComplete                = STRING(job-mch.mr-complete)
                   cRunComplete               = STRING(job-mch.run-complete)
                   cJobMchDueDate             = STRING(job-mch.due-date,"99/99/9999")
                   cJobMchDueTime             = STRING(job-mch.due-time)
                   cMRTotalRate               = STRING(job-mch.mr-trate)
                   cFixedOverHeadRate         = STRING(job-mch.run-fixoh)
                   cRunProfitPct              = STRING(job-mch.run-profit)
                   cMRProfitPct               = STRING(job-mch.mr-profit)
                   cRunRate                   = STRING(job-mch.run-rate)
                   cRunTotalRate              = STRING(job-mch.run-trate)
                   cVariableOverheadRate      = STRING(job-mch.run-varoh)
                   cMRContributionRate        = STRING(job-mch.mr-cont)
                   cRunContributionRate       = STRING(job-mch.run-cont)
                   cJobMachineID              = STRING(job-mch.job-mchID)
                   cJobMchWastePct            = STRING(job-mch.wst-prct)
                   . 
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "Anchored",cAnchored).   
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "Blank",cJobMchBlank).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "Department",cDepartment).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "DueDate",cJobMchDueDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "DueTime",cJobMchDueTime).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunEndDate",cEndDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "SetupEndDate",cSetupEndDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunEndTime",cEndTime).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "SetupEndtime",cSetupEndTime).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "Form",cJobMchForm).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "ItemName",cItemName).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "Item",cJobMchItem).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MachineID",cJobMachineID).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "LagTime",cLagTime).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "Line",cJobMchLineNumber).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MachineCode",cMachineCode).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MRContributionRate",cMRContributionRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MRFixedOHRate",cMchMRFixedOverheadRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MRRate",cMRRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MRTotalRate",cMRTotalRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MRVariableOverHeadRate",cMchMRvariableOverHeadRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MRWaste",cMRWaste).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "QueueTime",cQueueTime).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunContributionRate",cRunContributionRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "FixedOverHeadRate",cFixedOverHeadRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunHours",cRunHours).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunMinutes",cRunMinutes).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MakeReadyHours",cMRHours).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MakeReadyMinutes",cMRMinutes).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunQuantity",cRunQuantity).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunRate",cRunRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunVarOH",cVariableOverheadRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunTotalRate",cRunTotalRate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunSpeed",cRunSpeed).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunStartDate",cRunStartDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "SetupStartDate",cSetupStartDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunStartTime",cRunStartTime).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "SetupStartTime",cSetupStartTime).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "RunProfitPercentage",cRunProfitPct).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MRProfitPercentage",cMRProfitPct).
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "WastePercentage",cJobMchWastePct).
                
                lcConcatJobMachineData = lcConcatJobMachineData +  "~n" + lcJobMachineData.         
            END. 
        END.
        FIND FIRST bf-APIOutboundDetail NO-LOCK
             WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND bf-APIOutboundDetail.detailID      EQ "JobPreps"
               AND bf-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
             NO-ERROR. 
        IF AVAILABLE bf-APIOutboundDetail THEN DO:  
            FOR EACH job-prep NO-LOCK
                WHERE job-prep.company EQ job.company
                  AND job-prep.job-no  EQ job.job-no
                  AND job-prep.job-no2 EQ job.job-no2:
                      
                ASSIGN  
                    lcJobPrepData     = bf-APIOutboundDetail.data 
                    cPrepCode         = job-prep.code
                    cJobPrepQuantity  = STRING(job-prep.qty)
                    cItemStandardCost = STRING(job-prep.std-cost)
                    cStandardUOM      = job-prep.sc-uom
                    cCostPerM         = STRING(job-prep.cost-m)
                    cPrepDate         = STRING(job-prep.prep-date)
                    cPrepTime         = STRING(job-prep.prep-time)
                    cjobPrepPosted    = STRING(job-prep.opn)        
                    cJobPrepForm      = STRING(job-prep.frm)
                    cJobPrepBlank     = STRING(job-prep.blank-no)
                    cMatOrLab         = IF job-prep.ml THEN "Material" ELSE "Labor"
                    cSIMON            = IF job-prep.simon EQ "S"      THEN "Seperate" 
                                        ELSE IF job-prep.simon EQ "I" THEN "Integrate"
                                        ELSE IF job-prep.simon EQ "M" THEN "Maintenance" 
                                        ELSE IF job-prep.simon EQ "O" THEN "Other"
                                        ELSE IF job-prep.simon EQ "N" THEN "No Charge"
                                        ELSE job-prep.simon
                    .  
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "Blank",cJobPrepBlank).
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "PrepCode",cPrepCode). 
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "CostPerM",cCostPerM). 
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "Form",cJobPrepForm). 
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "PreperationDate",cPrepDate). 
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "PreperationTime",cPrepTime). 
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "Quantity",cJobPrepQuantity).
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "ItemStandardCost",cItemStandardCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "UOM",cStandardUOM).  
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "Posted",cJobPrepPosted).
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "SIMON",cSIMON).
                RUN updateRequestData(INPUT-OUTPUT lcJobPrepData, "MachineOrLabor",cMatOrLab).  
                
                lcConcatJobPrepData = lcConcatJobPrepData + "~n" + lcJobPrepData.                  
            END. 
        END.
        lcJobsData = APIOutboundDetail.data. 

        lcJobsData       = REPLACE(lcJobsData, "$JobHeader$", lcConcatJobHeaderData).
        lcJobsData       = REPLACE(lcJobsData, "$JobMaterial$", lcConcatJobMatData).
        lcJobsData       = REPLACE(lcJobsData, "$JobMachine$", lcConcatJobMachineData).
        lcJobsData       = REPLACE(lcJobsData, "$JobPrep$", lcConcatJobPrepData).

        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Company",cCompany).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobNumber1",cJobNo).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobNumber2",cJobNo2).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Priority",cPriority).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "IsPriority",cIsPriority).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Location",cLocation).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Estimate",cEstimate).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "CSRID",cCSRID).                
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "EnteredBy",cEnteredBy).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobDueDate",cJobDueDate).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobDueTime",cJobDueTime).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobStartDate",cJobStartDate).

        ioplcRequestData = REPLACE(ioplcRequestData, "$Jobs$", lcJobsData).   
    END.                        
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
