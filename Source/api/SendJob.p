
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
    
    /* Job Header Variables*/
    
    DEFINE VARIABLE cCompany                      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo                        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2                       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEstimate                     AS CHARACTER NO-UNDO.
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
    DEFINE VARIABLE cRunSpeed                   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cMRHours                    AS CHARACTER  NO-UNDO.
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
         
/**********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
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
                ASSIGN 
                    lcJobHeaderData               = bf-APIOutboundDetail.data
                    cCompany                      = job-hdr.company  
                    cJobNo                        = job-hdr.job-no
                    cJobNo2                       = STRING(job-hdr.job-no2)            
                    cLocation                     = job-hdr.loc    
                    cEstimate                     = job-hdr.est-no
                    cOrder                        = TRIM(STRING(job-hdr.ord-no,">>>>>9"))                                     
                    cStartDate                    = TRIM(STRING(job-hdr.start-date,"99/99/9999"))
                    cQuantity                     = TRIM(STRING(job-hdr.qty,">>,>>>,>>9"))                    cAverageCost                  = TRIM(STRING(job-hdr.avg-cost,">>>,>>9.9999"))                    cLastPurchaseCost             = TRIM(STRING(job-hdr.last-cost,">>>,>>9.9999"))                    cStandardItemCost             = TRIM(STRING(job-hdr.std-tot-cost,"->>>,>>9.99<<"))                    cStandardMaterialCost         = TRIM(STRING(job-hdr.std-mat-cost,"->>>,>>9.99<<"))                    cStandardLaborCost            = TRIM(STRING(job-hdr.std-lab-cost,"->>>,>>9.99<<"))                    cStandardFixedOverheadCost    = TRIM(STRING(job-hdr.std-fix-cost,"->>>,>>9.99<<"))                    cStandardVariableOverHeadCost = TRIM(STRING(job-hdr.std-var-cost,"->>>,>>9.99<<"))                    cItem                         = job-hdr.i-no                    cCustomer                     = job-hdr.cust-no                    cForm                         = TRIM(STRING(job-hdr.frm,">>9"))                    cBlank                        = TRIM(STRING(job-hdr.blank-no,">9"))                    cDueDate                      = TRIM(STRING(job-hdr.due-date,"99/99/9999"))                    cCustomerPO                   = job-hdr.po-no                    cDueTime                      = TRIM(STRING(job-hdr.due-time,">>>>9"))                    cFreezeNotes                  = STRING(job-hdr.freezeNote)                    cSplitShip                    = STRING(job-hdr.splitShip)
                    cSquareInchPct                = STRING(job-hdr.sq-in,">>9.99")                    cFreezeNotesDate              = STRING(job-hdr.freezeNotesDate,"99/99/9999")                    cWarehoused                   = STRING(job-hdr.whsed)
                    cOpened                       = STRING(job-hdr.opened)
                    cPrinted                      = STRING(job-hdr.ftick-prnt)
                    .
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "AverageCost",cAverageCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Blank",cBlank).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Customer",cCustomer). 
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "DueDate",cDueDate). 
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "DueTime",cDueTime). 
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Estimate",cEstimate).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "FreezeNotes",cFreezeNotes).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "FreezeNotesDate",cFreezeNotesDate).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Form",cForm).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Item",cItem).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "LastPurchaseCost",cLastPurchaseCost).
                RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Location",cLocation).
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
                    lcJobMatData               = bf-APIOutboundDetail.data
                    cItemNumber                = job-mat.i-no
                    cJobMatItemStandardCost    = TRIM(STRING(job-mat.std-cost,">>>,>>9.99<<"))
                    cStandardCostUOM           = job-mat.sc-uom
                    cCostPerUOM                = TRIM(STRING(job-mat.cost-m,"->,>>9.9999"))
                    cQtyToOrder                = TRIM(STRING(job-mat.qty,">,>>>,>>9.9<<<<<"))
                    cQuantityUOM               = job-mat.qty-uom
                    cLength                    = TRIM(STRING(job-mat.len,">>9.99<<"))
                    cWidth                     = TRIM(STRING(job-mat.wid,">>9.99<<"))
                    cBasisWeight               = TRIM(STRING(job-mat.basis-w,">>9.99"))
                    cLineNumber                = TRIM(STRING(job-mat.line,"99"))
                    cRMItem                    = job-mat.rm-i-no
                    cJobMatBlank               = TRIM(STRING(job-mat.blank-no,">9"))
                    cJobMatForm                = TRIM(STRING(job-mat.frm,">>9"))
                    cAllocated                 = STRING(all-flg)
                    cQuantityAllocated         = TRIM(STRING(job-mat.qty-all,">>>,>>9.99<<<<"))
                    cQuantityIssued            = TRIM(STRING(job-mat.qty-iss,"->>,>>9.99<<<<"))
                    cMRQuantity                = TRIM(STRING(job-mat.qty-mr,">>>>9.99<<<<"))
                    cWasteQuantity             = TRIM(STRING(job-mat.qty-wst,">>>>9.99<<<<"))
                    cDepth                     = TRIM(STRING(job-mat.dep,">,>>9.99<<<<"))
                    cPONumber                  = TRIM(STRING(job-mat.po-no,">>>>>9"))
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
                   cMRWaste                   = TRIM(STRING(job-mch.mr-waste,">>>9"))
                   cMRRate                    = TRIM(STRING(job-mch.mr-rate,">>9.99"))
                   cMchMRvariableOverHeadRate = TRIM(STRING(job-mch.mr-varoh,">>9.99"))
                   cMchMRFixedOverheadRate    = TRIM(STRING(job-mch.mr-fixoh,">>9.99"))
                   cDepartment                = job-mch.dept
                   cItemName                  = job-mch.i-name
                   cJobMchItem                = job-mch.i-no
                   cJobMatForm                = TRIM(STRING(job-mch.frm,">>9"))
                   cRunHours                  = TRIM(STRING(job-mch.run-hr,">>9.99"))
                   cRunSpeed                  = TRIM(STRING(job-mch.speed, ">>>>9"))
                   cMRHours                   = TRIM(STRING(job-mch.mr-hr,">>9.99"))
                   cJobMchBlank               = TRIM(STRING(job-mch.blank-no,">9"))
                   cJobMchLineNumber          = TRIM(STRING(job-mch.line,">9"))
                   cRunQuantity               = TRIM(STRING(job-mch.run-qty,">,>>>,>>9.9<<"))
                   cRunStartDate              = TRIM(STRING(job-mch.start-date,"99/99/9999"))
                   cRunStartTime              = TRIM(STRING(job-mch.start-time,"->,>>>,>>9"))
                   cQueueTime                 = TRIM(STRING(job-mch.queue-time,">>>>9"))
                   cLagTime                   = TRIM(STRING(job-mch.lag-time,">>>>9"))
                   cEndDate                   = STRING(job-mch.end-date,"99/99/9999")
                   cEndTime                   = TRIM(STRING(job-mch.end-time,"->,>>>,>>9"))
                   cSetupStartDate            = TRIM(STRING(job-mch.start-date-su,"99/99/9999"))
                   cSetupStartTime            = TRIM(STRING(job-mch.start-time-su,"->,>>>,>>9"))
                   cSetupEndDate              = STRING(job-mch.end-date-su,"99/99/9999")
                   cSetupEndTime              = TRIM(STRING(job-mch.end-time-su,"->,>>>,>>9"))
                   cMRComplete                = STRING(job-mch.mr-complete)
                   cRunComplete               = STRING(job-mch.run-complete)
                   cJobMchDueDate             = STRING(job-mch.due-date,"99/99/9999")
                   cJobMchDueTime             = TRIM(STRING(job-mch.due-time,">>>>9"))
                   cMRTotalRate               = TRIM(STRING(job-mch.mr-trate,">>9.99"))
                   cFixedOverHeadRate         = TRIM(STRING(job-mch.run-fixoh,">>9.99"))
                   cRunProfitPct              = TRIM(STRING(job-mch.run-profit,">>9.99"))
                   cMRProfitPct               = TRIM(STRING(job-mch.mr-profit,">>9.99"))
                   cRunRate                   = TRIM(STRING(job-mch.run-rate,">>9.99"))
                   cRunTotalRate              = TRIM(STRING(job-mch.run-trate,">>9.99"))
                   cVariableOverheadRate      = TRIM(STRING(job-mch.run-varoh,">>9.99"))
                   cMRContributionRate        = TRIM(STRING(job-mch.mr-cont,">>9.99"))
                   cRunContributionRate       = TRIM(STRING(job-mch.run-cont,">>9.99"))
                   cJobMachineID              = TRIM(STRING(job-mch.job-mchID,">>>>>>9"))
                   cJobMchWastePct            = TRIM(STRING(job-mch.wst-prct,">>9.99"))
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
                RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "MakeReadyHours",cMRHours).
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
                    cJobPrepQuantity  = TRIM(STRING(job-prep.qty,"->>>,>>>,>>9.9<<<<<"))
                    cItemStandardCost = TRIM(STRING(job-prep.std-cost,">>>,>>9.99<<"))
                    cStandardUOM      = job-prep.sc-uom
                    cCostPerM         = TRIM(STRING(job-prep.cost-m,"->,>>9.9999"))
                    cPrepDate         = TRIM(STRING(job-prep.prep-date,"99/99/9999"))
                    cPrepTime         = TRIM(STRING(job-prep.prep-time,"->,>>>,>>9"))
                    cjobPrepPosted    = STRING(job-prep.opn)        
                    cJobPrepForm      = TRIM(STRING(job-prep.frm,">>9"))
                    cJobPrepBlank     = TRIM(STRING(job-prep.blank-no,">9"))
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
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Company",cCompany).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobNumber1",cJobNo).
        RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobNumber2",cJobNo2).

        lcJobsData       = REPLACE(lcJobsData, "$JobHeader$", lcConcatJobHeaderData).
        lcJobsData       = REPLACE(lcJobsData, "$JobMaterial$", lcConcatJobMatData).
        lcJobsData       = REPLACE(lcJobsData, "$JobMachine$", lcConcatJobMachineData).
        lcJobsData       = REPLACE(lcJobsData, "$JobPrep$", lcConcatJobPrepData).
        ioplcRequestData = REPLACE(ioplcRequestData, "$Jobs$", lcJobsData).   
    END.                        
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .