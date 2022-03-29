
/*------------------------------------------------------------------------
    File        : JobSumReport.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Sewa Singh
    Created     : Thur Jan 06 2022
    Notes       :
  ----------------------------------------------------------------------*/
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */  

/* ***************************  Definitions  ************************** */
{jc\ttJobReport.i}

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFromJobNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiFromJobNo2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipcToJobNo AS CHARACTER NO-UNDO.   
DEFINE INPUT PARAMETER ipiToJobNo2 AS INTEGER NO-UNDO.  
DEFINE INPUT PARAMETER ipcStatus AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdtCloseStart AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipdtCloseEnd AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttJob.
DEFINE OUTPUT PARAMETER TABLE FOR ttDepartment.
DEFINE OUTPUT PARAMETER TABLE FOR ttOperation.
DEFINE OUTPUT PARAMETER TABLE FOR ttMaterial.
DEFINE OUTPUT PARAMETER TABLE FOR ttItem.


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */
FUNCTION fGetOnHandQty RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER,
    ipcJobNo AS CHARACTER,
    ipiJobNo2 AS INTEGER,
    ipcFGItem AS CHARACTER) FORWARD.
    
/* ***************************  Main Block  *************************** */
          
RUN pBuildTempTables(ipcCompany, ipcFromJobNo, ipiFromJobNo2, ipcToJobNo, ipiToJobNo2, ipcStatus, ipdtCloseStart, ipdtCloseEnd).

/* **********************  Internal Procedures  *********************** */
PROCEDURE pBuildTempTables PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFromJobNo2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToJobNo AS CHARACTER  NO-UNDO.    
    DEFINE INPUT PARAMETER ipiToJobNo2 AS INTEGER  NO-UNDO.
    DEFINE INPUT PARAMETER ipcStatus AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtCloseStart AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipdtCloseEnd AS DATE NO-UNDO.
    DEFINE VARIABLE cJobNo LIKE job.job-no EXTENT 2 INIT [" ", "zzzzzzzzz"] NO-UNDO.
    DEFINE VARIABLE lClosedOnly AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lOpenOnly AS LOGICAL NO-UNDO.
    
    ASSIGN
        cJobNo[1] = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', ipcFromJobNo, ipiFromJobNo2)) 
        cJobNo[2] = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', ipcToJobNo, ipiToJobNo2)) 
        lClosedOnly = ipcStatus EQ "C"
        lOpenOnly = ipcStatus EQ "O"
        . 
    
    FOR EACH job NO-LOCK 
        WHERE job.company EQ ipcCompany         
        AND fill(" ",9 - length(trim(job.job-no))) +
        trim(job.job-no) + string(int(job.job-no2),"999") GE cJobNo[1]
        AND fill(" ",9 - length(trim(job.job-no))) +
        trim(job.job-no) + string(int(job.job-no2),"999") LE cJobNo[2]
        AND job.job-no2 GE int(ipiFromJobNo2)
        AND job.job-no2 LE int(ipiToJobNo2)
        AND ((lClosedOnly AND job.stat EQ 'C' AND job.close-date GE ipdtCloseStart AND job.close-date LE ipdtCloseEnd) 
            OR (lOpenOnly AND job.stat NE 'C')
            OR (NOT lClosedOnly AND NOT lOpenOnly)),  
        FIRST estCostHeader NO-LOCK 
        WHERE estCostHeader.company EQ job.company
        AND estCostHeader.jobID EQ job.job-no
        AND estCostHeader.jobID2 EQ job.job-no2:  
                
        CREATE ttJob.
        ASSIGN                
            ttJob.cJobNo      = job.job-no
            ttJob.iJobNo2     = job.job-no2
            ttJob.dtCloseDate = job.close-date
            ttJob.cEstimate   = job.est-no.  
                     
        RUN pBuildFGItems(BUFFER ttJob, BUFFER job).
        RUN pBuildDepartmentsAndOperations(BUFFER job, BUFFER estCostHeader).
        RUN pBuildMaterials(BUFFER job, BUFFER estCostHeader).          
    END.
END PROCEDURE.

PROCEDURE pBuildFGItems PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttJob FOR ttJob.
    DEFINE PARAMETER BUFFER ipbf-job   FOR job.
       
    FOR EACH job-hdr NO-LOCK 
        WHERE job-hdr.company EQ ipbf-job.company
        AND job-hdr.job-no EQ ipbf-job.job-no
        AND job-hdr.job-no2 EQ ipbf-job.job-no2
        AND job-hdr.job EQ ipbf-job.job
        ,
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no EQ job-hdr.i-no,
        FIRST cust NO-LOCK 
        WHERE cust.company EQ job-hdr.company
        AND cust.cust-no EQ job-hdr.cust-no: 
        
        CREATE ttItem.
        ASSIGN            
            ttItem.cJobNo        = ipbf-job.job-no
            ttItem.iJobNo2       = ipbf-job.job-no2               
            ttItem.iFormNo       = job-hdr.frm
            ttItem.iBlankNo      = job-hdr.blank-no
            ttItem.cFGItem       = job-hdr.i-no
            ttItem.cFGName       = itemfg.i-name
            ttItem.dSellingPrice = itemfg.sell-price
            ttItem.cSellingUom   = itemfg.sell-uom
            ttItem.dJobQty       = job-hdr.qty
            ttItem.cItemDesc     = itemfg.part-dscr1
            ttItem.cProductCat   = itemfg.procat
            ttItem.dStdCost      = job-hdr.std-mat-cost + job-hdr.std-lab-cost + job-hdr.std-fix-cost + job-hdr.std-var-cost
            .
         
        RUN fg/GetProductionQty.p (INPUT job-hdr.company,
            INPUT job-hdr.job-no,
            INPUT job-hdr.job-no2,
            INPUT job-hdr.i-no,
            INPUT NO,
            OUTPUT ttItem.dProduced).
                                
        ttItem.dOnHand   = fGetOnHandQty(job-hdr.company, job-hdr.job-no,job-hdr.job-no2,job-hdr.i-no).
                
        ipbf-ttJob.cCustName       = cust.NAME.
    END.
    
END PROCEDURE.

PROCEDURE pBuildDepartmentsAndOperations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job           FOR job.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    
    DEFINE VARIABLE dStdRunQty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdSetupHrs   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdRunHrs     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdSpeed      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdSetupWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdRunWaste   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdCost       AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dActRunQty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActSetupHrs   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActRunHrs     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActSpeed      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActSetupWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActRunWaste   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActCost       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iBlankNo       AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ttOperation FOR ttOperation.
         
    IF AVAILABLE ipbf-estCostHeader THEN
    DO:
        FOR EACH estCostOperation NO-LOCK 
            WHERE estCostOperation.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID,
            FIRST job-mch NO-LOCK 
            WHERE job-mch.company EQ ipbf-estCostHeader.company
            AND job-mch.job-no EQ ipbf-estCostHeader.jobID
            AND job-mch.job-no2 EQ ipbf-estCostHeader.jobID2
            AND job-mch.frm EQ estCostOperation.formNo
            AND job-mch.blank-no EQ estCostOperation.blankNo
            AND job-mch.pass EQ estCostOperation.pass
            AND job-mch.m-code EQ estCostOperation.operationID
            BY estCostOperation.sequenceOfOperation:
            CREATE ttOperation.
            ASSIGN                
                ttOperation.cJobNo        = ipbf-job.job-no
                ttOperation.iJobNo2       = ipbf-job.job-no2                                                      
                ttOperation.iFormNo       = estCostOperation.formNo
                ttOperation.iBlankNo      = estCostOperation.blankNo
                ttOperation.cDept         = estCostOperation.departmentIDPrimary
                ttOperation.cMachine      = estCostOperation.operationID
                ttOperation.cStdAct       = "Standard"
                ttOperation.dRunQty       = estCostOperation.quantityIn                    
                ttOperation.dSetupHrs     = estCostOperation.hoursSetup                   
                ttOperation.dRunHrs       = estCostOperation.hoursRun                  
                ttOperation.dSpeed        = estCostOperation.speed
                ttOperation.dCost         = estCostOperation.costTotal
                ttOperation.dSetupWaste   = estCostOperation.quantityInSetupWaste                 
                ttOperation.dRunWaste     = estCostOperation.quantityInRunWaste                   
                ttOperation.iSeq          = estCostOperation.sequence
                ttOperation.iPass         = estCostOperation.pass
                ttOperation.dMRCrew       = estCostOperation.crewSizeSetup
                ttOperation.dRunCrew      = estCostOperation.crewSizeRun
                .
        END.           
    END.
    
    RUN pBuildMiscPrep(BUFFER ipbf-job, BUFFER ipbf-estCostHeader, "Lab").
    
    FOR EACH mch-act NO-LOCK
        WHERE mch-act.company EQ ipbf-job.company
        AND mch-act.job-no EQ ipbf-job.job-no
        AND mch-act.job-no EQ ipbf-job.job-no,
        FIRST mach NO-LOCK 
        WHERE mach.company EQ mch-act.company
        AND mach.m-code  EQ mch-act.m-code,
        FIRST job-code NO-LOCK 
        WHERE job-code.code EQ mch-act.code:
        iBlankNo = IF mach.p-type EQ "B" AND mch-act.blank-no EQ 0 THEN 1 ELSE mch-act.blank-no.
        FIND FIRST ttOperation
            WHERE ttOperation.cJobNo EQ ipbf-job.job-no
            AND ttOperation.iJobNo2 EQ ipbf-job.job-no2
            AND ttOperation.cMachine EQ mch-act.m-code
            AND ttOperation.iFormNo EQ mch-act.frm
            AND ttOperation.iBlankNo EQ iBlankNo
            AND ttOperation.iPass EQ mch-act.pass
            AND ttOperation.cStdAct EQ "Actual"
            NO-ERROR.
        IF NOT AVAILABLE ttOperation THEN DO:  
            CREATE ttOperation.
            ASSIGN
                ttOperation.cJobNo        = ipbf-job.job-no
                ttOperation.iJobNo2       = ipbf-job.job-no2                                              
                ttOperation.iFormNo       = mch-act.frm
                ttOperation.iBlankNo      = iBlankNo
                ttOperation.cDept         = mch-act.dept
                ttOperation.cMachine      = mch-act.m-code
                ttOperation.cStdAct       = "Actual"
                ttOperation.iPass         = mch-act.pass
                .
        END.
        CASE job-code.cat:
            WHEN "RUN" THEN DO:
                ASSIGN
                    ttOperation.dRunCrew      = ttOperation.dRunCrew + mch-act.crew
                    ttOperation.dRunQty       = ttOperation.dRunQty + mch-act.qty
                    ttOperation.dRunHrs       = ttOperation.dRunHrs + mch-act.hours
                    ttOperation.dRunWaste     = ttOperation.dRunWaste + mch-act.waste
                    ttOperation.dSpeed        = ttOperation.dRunQty / ttOperation.dRunHrs          
                    ttOperation.dCost         = ttOperation.dCost + (mch-act.hours *  mach.run-rate) + (mach.run-fixoh  * mch-act.hours)
                                                                  + (mach.run-varoh * mch-act.hours) .          
                   
            END.
            WHEN "MR" THEN DO:
                ASSIGN 
                    ttOperation.dMRCrew       = ttOperation.dMRCrew + mch-act.crew
                    ttOperation.dSetupHrs     = ttOperation.dSetupHrs + mch-act.hours                   
                    ttOperation.dSetupWaste   = ttOperation.dSetupWaste + mch-act.waste
                    ttOperation.dCost         = ttOperation.dCost + (mch-act.hours *  mach.mr-rate) + (mach.mr-fixoh * mch-act.hours)
                                                                  + (mach.mr-varoh * mch-act.hours) 
                    .
            END.
            OTHERWISE DO:
                ASSIGN 
                    ttOperation.cDTChargeable = ttOperation.cDTChargeable + mch-act.d-type + ","
                    ttOperation.dDownTimeHrs  = ttOperation.dDowntimeHrs + mch-act.hours
                    .    
            END.
        END CASE.
                
    END.  
    
    FOR EACH ttOperation NO-LOCK
        WHERE ttOperation.cJobNo EQ ipbf-job.job-no
        AND ttOperation.iJobNo2 EQ ipbf-job.job-no2
        BREAK BY ttOperation.cJobNo
        BY ttOperation.iJobNo2
        BY ttOperation.cDept
        BY ttOperation.iPass
        BY ttOperation.iFormNo
        BY ttOperation.iBlankNo:
        
        FIND FIRST bf-ttOperation
            WHERE bf-ttOperation.cJobNo EQ ttOperation.cJobNo
            AND bf-ttOperation.iJobNo2 EQ ttOperation.iJobNo2
            AND bf-ttOperation.cMachine EQ ttOperation.cMachine
            AND bf-ttOperation.iFormNo EQ ttOperation.iFormNo
            AND bf-ttOperation.iBlankNo EQ ttOperation.iBlankNo
            AND bf-ttOperation.iPass EQ ttOperation.iPass
            AND bf-ttOperation.cStdAct EQ "Actual"
            NO-ERROR.
        IF NOT AVAIL bf-ttOperation THEN
        DO:
              CREATE bf-ttOperation.
              ASSIGN
                bf-ttOperation.cJobNo        = ttOperation.cJobNo
                bf-ttOperation.iJobNo2       = ttOperation.iJobNo2                                              
                bf-ttOperation.iFormNo       = ttOperation.iFormNo
                bf-ttOperation.iBlankNo      = ttOperation.iBlankNo
                bf-ttOperation.cDept         = ttOperation.cDept
                bf-ttOperation.cMachine      = ttOperation.cMachine
                bf-ttOperation.cStdAct       = "Actual"
                bf-ttOperation.iPass         = ttOperation.iPass.              
        END.
               
        IF FIRST-OF(ttOperation.iBlankNo) THEN
            ASSIGN
                dActRunQty     = 0            
                dActSetupHrs   = 0                   
                dActRunHrs     = 0                  
                dActSpeed      = 0
                dActSetupWaste = 0                 
                dActRunWaste   = 0 
                dActCost       = 0
                dStdRunQty     = 0            
                dStdSetupHrs   = 0                   
                dStdRunHrs     = 0                  
                dStdSpeed      = 0
                dStdSetupWaste = 0                 
                dStdRunWaste   = 0  
                dStdCost       = 0.
        
        
        FIND FIRST ttDepartment NO-LOCK
            WHERE ttDepartment.cJobNo EQ ipbf-job.job-no
            AND ttDepartment.iJobNo2 EQ ipbf-job.job-no2
            AND ttDepartment.cDept EQ ttOperation.cDept 
            AND ttDepartment.iFormNo EQ ttOperation.iFormNo
            AND ttDepartment.iBlankNo EQ ttOperation.iBlankNo
            AND ttDepartment.iPass EQ ttOperation.iPass
            NO-ERROR.
        IF NOT AVAIL ttDepartment THEN
        DO:
            CREATE ttDepartment.
            ASSIGN
                ttDepartment.cJobNo  = ipbf-job.job-no
                ttDepartment.iJobNo2 = ipbf-job.job-no2
                ttDepartment.cDept   = ttOperation.cDept
                ttDepartment.iFormNo = ttOperation.iFormNo
                ttDepartment.iBlankNo = ttOperation.iBlankNo
                ttDepartment.iSeq     = ttOperation.iSeq
                ttDepartment.iPass    = ttOperation.iPass.            
        END.          
        ASSIGN            
            ttDepartment.dRunQty       = ttDepartment.dRunQty + ttOperation.dRunQty            
            ttDepartment.dSetupHrs     = ttDepartment.dSetupHrs + ttOperation.dSetupHrs                   
            ttDepartment.dRunHrs       = ttDepartment.dRunHrs + ttOperation.dRunHrs                  
            ttDepartment.dSpeed        = ttDepartment.dSpeed + ttOperation.dSpeed
            ttDepartment.dSetupWaste   = ttDepartment.dSetupWaste + ttOperation.dSetupWaste                 
            ttDepartment.dRunWaste     = ttDepartment.dRunWaste + ttOperation.dRunWaste  
            ttDepartment.dCost         = ttDepartment.dCost + ttOperation.dCost
            ttDepartment.cDTChargeable = ttOperation.cDTChargeable
            ttDepartment.dDownTimeHrs  = ttDepartment.dDownTimeHrs + ttOperation.dDownTimeHrs
            .        
        
        IF ttOperation.cStdAct EQ "Actual" THEN
            assign
                ttJob.dTotActMachineCost = ttJob.dTotActMachineCost + ttOperation.dCost
                ttJob.dTotActCost        = ttJob.dTotActCost + ttOperation.dCost                     
                dActRunQty               = dActRunQty + ttOperation.dRunQty            
                dActSetupHrs             = dActSetupHrs + ttOperation.dSetupHrs                   
                dActRunHrs               = dActRunHrs + ttOperation.dRunHrs                  
                dActSpeed                = dActSpeed + ttOperation.dSpeed
                dActSetupWaste           = dActSetupWaste + ttOperation.dSetupWaste                 
                dActRunWaste             = dActRunWaste + ttOperation.dRunWaste  
                dActCost                 = dActCost + ttOperation.dCost
                .
        IF ttOperation.cStdAct EQ "Standard" THEN 
            assign
                ttJob.dTotStdMachineCost = ttJob.dTotStdMachineCost + ttOperation.dCost
                ttJob.dTotStdCost        = ttJob.dTotStdCost + ttOperation.dCost
                dStdRunQty               = dStdRunQty + ttOperation.dRunQty            
                dStdSetupHrs             = dStdSetupHrs + ttOperation.dSetupHrs                   
                dStdRunHrs               = dStdRunHrs + ttOperation.dRunHrs                  
                dStdSpeed                = dStdSpeed + ttOperation.dSpeed
                dStdSetupWaste           = dStdSetupWaste + ttOperation.dSetupWaste                 
                dStdRunWaste             = dStdRunWaste + ttOperation.dRunWaste  
                dStdCost                 = dStdCost + ttOperation.dCost
                .
             
        IF LAST-OF(ttOperation.iBlankNo) THEN
        DO:
            ASSIGN
                ttDepartment.dRunQtyVar     = (dStdRunQty - dActRunQty) / dStdRunQty * 100 
                ttDepartment.dSetupHrsVar   = (dStdSetupHrs - dActSetupHrs) / dStdSetupHrs * 100                   
                ttDepartment.dRunHrsVar     = (dStdRunHrs - dActRunHrs) / dStdRunHrs * 100                   
                ttDepartment.dSpeedVar      = (dStdSpeed - dActSpeed) / dStdSpeed * 100 
                ttDepartment.dSetupWasteVar = (dStdSetupWaste - dActSetupWaste) / dStdSetupWaste * 100                  
                ttDepartment.dRunWasteVar   = (dStdRunWaste - dActRunWaste) / dStdRunWaste * 100   
                ttDepartment.dCostVar       = (dStdCost - dActCost) / dStdCost * 100 
                .    
            IF ttDepartment.dRunQtyVar EQ ? THEN ttDepartment.dRunQtyVar = 0. 
            IF ttDepartment.dSetupHrsVar EQ ? THEN ttDepartment.dSetupHrsVar = 0.
            IF ttDepartment.dRunHrsVar EQ ? THEN ttDepartment.dRunHrsVar = 0.
            IF ttDepartment.dSpeedVar EQ ? THEN ttDepartment.dSpeedVar = 0.
            IF ttDepartment.dSetupWasteVar EQ ? THEN ttDepartment.dSetupWasteVar = 0.
            IF ttDepartment.dRunWasteVar EQ ? THEN ttDepartment.dRunWasteVar = 0.
            IF ttDepartment.dCostVar EQ ? THEN ttDepartment.dCostVar = 0.
        END.
    END.      
   
END PROCEDURE.

PROCEDURE pBuildMaterials PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job           FOR job.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    
    DEFINE VARIABLE dStdItemQty   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdItemCost  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActItemQty   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActItemCost  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMaterialCost AS DECIMAL NO-UNDO.
     
    IF AVAILABLE ipbf-estCostHeader THEN
    DO:
        FOR EACH estCostMaterial NO-LOCK 
            WHERE estCostMaterial.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID,
            FIRST materialType NO-LOCK  /*materialType for AutoIssue logic*/
            WHERE materialType.company EQ estCostMaterial.company
            AND materialType.materialType EQ estCostMaterial.materialType,
            FIRST job-mat NO-LOCK /*Confirm job-mat has not been deleted from JU1*/
            WHERE job-mat.company EQ ipbf-estCostHeader.company
            AND job-mat.job-no EQ ipbf-estCostHeader.jobID
            AND job-mat.job-no2 EQ ipbf-estCostHeader.jobID2
            AND job-mat.frm EQ estCostMaterial.formNo
            AND job-mat.blank-no EQ estCostMaterial.blankNo
            AND job-mat.i-no EQ estCostMaterial.itemID
            BREAK BY estCostMaterial.formNo DESCENDING
            BY estCostMaterial.blankNo
            BY estCostMaterial.itemID:
            
            FIND FIRST ttMaterial NO-LOCK
                WHERE ttMaterial.cJobNo EQ ipbf-job.job-no
                AND ttMaterial.iJobNo2 EQ ipbf-job.job-no2
                AND ttMaterial.cMaterial EQ estCostMaterial.itemID
                NO-ERROR.
                
            IF NOT AVAILABLE ttMaterial THEN
            DO:
                CREATE ttMaterial.
                ASSIGN
                    ttMaterial.cJobNo    = ipbf-job.job-no
                    ttMaterial.iJobNo2   = ipbf-job.job-no2 
                    ttMaterial.iFormNo   = estCostMaterial.formNo
                    ttMaterial.iBlankNo  = estCostMaterial.blankNo                             
                    ttMaterial.cMaterial = estCostMaterial.itemID
                    ttMaterial.cStdUom   = estCostMaterial.quantityUOM 
                    ttMaterial.lAutoIssue = materialType.autoIssue.
            END.
            ASSIGN 
                ttMaterial.dQtyStd  = ttMaterial.dQtyStd + estCostMaterial.quantityRequiredTotal
                ttMaterial.dCostStd = ttMaterial.dCostStd + estCostMaterial.costTotal
                ttMaterial.cUsedonForms = ttMaterial.cUsedonForms + (IF LOOKUP(string(estCostMaterial.formNo),ttMaterial.cUsedonForms) EQ 0 THEN STRING(estCostMaterial.formNo) + "," ELSE "")
                .                   
        END.
        RUN pBuildMiscPrep(BUFFER ipbf-job, BUFFER ipbf-estCostHeader, "Mat").
        
    END.
    FOR EACH mat-act NO-LOCK
        WHERE mat-act.company EQ ipbf-job.company
        AND mat-act.job-no EQ ipbf-job.job-no
        AND mat-act.job-no2 EQ ipbf-job.job-no2:
                
        FIND FIRST ttMaterial NO-LOCK
            WHERE ttMaterial.cJobNo EQ ipbf-job.job-no
            AND ttMaterial.iJobNo2 EQ ipbf-job.job-no2
            AND ttMaterial.cMaterial EQ mat-act.i-no
            NO-ERROR.
                
        IF NOT AVAILABLE ttMaterial THEN
        DO:
            CREATE ttMaterial.
            ASSIGN
                ttMaterial.cJobNo    = ipbf-job.job-no
                ttMaterial.iJobNo2   = ipbf-job.job-no2 
                ttMaterial.iFormNo   = mat-act.s-num
                ttMaterial.iBlankNo  = mat-act.b-num                             
                ttMaterial.cMaterial = mat-act.i-no
                ttMaterial.cActUom   = mat-act.qty-uom.
        END.
        ASSIGN
            ttMaterial.cActUom  = mat-act.qty-uom
            ttMaterial.dQtyAct  = ttMaterial.dQtyAct + mat-act.qty.
            RUN pGetMaterialCost(BUFFER ipbf-job, BUFFER ITEM, BUFFER mat-act, OUTPUT dMaterialCost). 
        ASSIGN    
            ttMaterial.dCostAct = ttMaterial.dCostAct +  ( mat-act.qty * dMaterialCost )
            ttMaterial.cUsedonForms = ttMaterial.cUsedonForms + (IF LOOKUP(string(mat-act.s-num),ttMaterial.cUsedonForms) EQ 0 THEN STRING(mat-act.s-num) + "," ELSE "")
            .            
    END.
    FOR EACH ttMaterial:     
        IF ttMaterial.lAutoIssue AND ttMaterial.dQtyAct EQ 0 THEN  /*Auto assign those items that will be auto issued eq to standards*/ 
            ASSIGN 
                ttMaterial.dQtyAct = ttMaterial.dQtyStd
                ttMaterial.dCostAct = ttMaterial.dCostStd
                ttMaterial.cActUom = ttMaterial.cStdUom
                .   
        ASSIGN 
            ttJob.dTotStdMaterialCost = ttJob.dTotStdMaterialCost + ttMaterial.dCostStd
            ttJob.dTotStdCost         = ttJob.dTotStdCost + ttMaterial.dCostStd
            ttJob.dTotActCost         = ttJob.dTotActCost + ttMaterial.dCostAct
            ttJob.dTotActMaterialCost = ttJob.dTotActMaterialCost + ttMaterial.dCostAct
            ttMaterial.dCostVar       = IF ttMaterial.dCostStd NE 0 THEN (ttMaterial.dCostStd - ttMaterial.dCostAct) / ttMaterial.dCostStd * 100 ELSE 0
            ttMaterial.dQtyVar        = IF ttMaterial.dQtyStd NE 0 THEN (ttMaterial.dQtyStd - ttMaterial.dQtyAct) / ttMaterial.dQtyStd * 100 ELSE 0
            .
  
    END.
      
END PROCEDURE.

PROCEDURE pBuildMiscPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE PARAMETER BUFFER ipbf-job           FOR job.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT PARAMETER ipcCostType AS CHARACTER NO-UNDO.
    
    FOR EACH estCostMisc NO-LOCK 
        WHERE estCostMisc.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID 
        AND estCostMisc.costType EQ ipcCostType
        BY estCostMisc.formNo
        BY estCostMisc.blankNo:
        
        IF estCostMisc.costType EQ "Mat" THEN
        DO:       
            FIND FIRST ttMaterial NO-LOCK
                 WHERE ttMaterial.cJobNo EQ ipbf-job.job-no
                   AND ttMaterial.iJobNo2 EQ ipbf-job.job-no2
                   AND ttMaterial.cMaterial EQ estCostMisc.itemID
                   NO-ERROR.
                    
                IF NOT AVAILABLE ttMaterial THEN
                DO:
                    CREATE ttMaterial.
                    ASSIGN
                        ttMaterial.cJobNo    = ipbf-job.job-no
                        ttMaterial.iJobNo2   = ipbf-job.job-no2 
                        ttMaterial.iFormNo   = estCostMisc.formNo
                        ttMaterial.iBlankNo  = estCostMisc.blankNo                             
                        ttMaterial.cMaterial = estCostMisc.itemID
                        ttMaterial.cStdUom   = estCostMisc.costUOM .
                END.
                ASSIGN 
                    ttMaterial.dQtyStd  = ttMaterial.dQtyStd + estCostMisc.quantityRequiredTotal
                    ttMaterial.dCostStd = ttMaterial.dCostStd + estCostMisc.costTotal
                    ttMaterial.cUsedonForms = ttMaterial.cUsedonForms + (IF LOOKUP(string(estCostMisc.formNo),ttMaterial.cUsedonForms) EQ 0 THEN STRING(estCostMisc.formNo) + "," ELSE "")
                    .         
        END.        
        ELSE IF estCostMisc.costType EQ "Lab" THEN
        DO:       
            CREATE ttOperation.
            ASSIGN                
                ttOperation.cJobNo        = ipbf-job.job-no
                ttOperation.iJobNo2       = ipbf-job.job-no2                                                      
                ttOperation.iFormNo       = estCostMisc.formNo
                ttOperation.iBlankNo      = estCostMisc.blankNo                 
                ttOperation.cMachine      = estCostMisc.costDescription 
                ttOperation.cStdAct       = "Standard"
                ttOperation.dRunQty       = estCostMisc.quantityRequiredTotal                
                ttOperation.dCost         = estCostMisc.costTotal                  
                .
        END.
    END.    

END PROCEDURE.    
 
PROCEDURE pGetMaterialCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job            FOR job.
    DEFINE PARAMETER BUFFER ipbf-item           FOR ITEM.
    DEFINE PARAMETER BUFFER ipbf-mat-act        FOR mat-act.
    DEFINE OUTPUT PARAMETER opdMaterialCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE cScUom AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    FIND FIRST job-mat NO-LOCK
         WHERE job-mat.company eq ipbf-job.company
           AND job-mat.job     eq ipbf-job.job 
           AND job-mat.i-no    eq ipbf-item.i-no NO-ERROR.
    
    cScUom = IF AVAIL job-mat AND job-mat.sc-uom gt "" THEN
                  job-mat.sc-uom else ipbf-mat-act.qty-uom.
    
    RUN Conv_ValueFromUOMtoUOM(ipbf-job.company, 
                    ipbf-item.i-no, 
                    "RM", 
                    DEC(ipbf-mat-act.cost),
                    cScUom, 
                    ipbf-mat-act.qty-uom, 
                    (IF AVAIL job-mat AND job-mat.basis-w ne 0 then job-mat.basis-w
                           ELSE ipbf-item.basis-w),
                    (IF AVAIL job-mat AND job-mat.len     ne 0 THEN job-mat.len
                           ELSE ipbf-item.s-len),
                    (IF AVAIL job-mat AND job-mat.wid     ne 0 THEN job-mat.wid
                           ELSE IF item.r-wid EQ 0 THEN ipbf-item.s-wid ELSE ipbf-item.r-wid),
                    ipbf-item.s-dep,
                    0, 
                    OUTPUT opdMaterialCost, 
                    OUTPUT lError, 
                    OUTPUT cMessage).                          
END PROCEDURE.    

/*PROCEDURE pMaterial PRIVATE:                                                                           */
/*    /*------------------------------------------------------------------------------                   */
/*     Purpose:                                                                                          */
/*     Notes:                                                                                            */
/*    ------------------------------------------------------------------------------*/                   */
/*    DEFINE PARAMETER BUFFER ipbf-job           FOR job.                                                */
/*    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.                                      */
/*                                                                                                       */
/*    DEFINE VARIABLE dStdItemQty  AS DECIMAL NO-UNDO.                                                   */
/*    DEFINE VARIABLE dStdItemCost AS DECIMAL NO-UNDO.                                                   */
/*    DEFINE VARIABLE dActItemQty  AS DECIMAL NO-UNDO.                                                   */
/*    DEFINE VARIABLE dActItemCost AS DECIMAL NO-UNDO.                                                   */
/*                                                                                                       */
/*    IF AVAILABLE ipbf-estCostHeader THEN                                                               */
/*    DO:                                                                                                */
/*        FOR EACH estCostMaterial NO-LOCK                                                               */
/*            WHERE estCostMaterial.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID                */
/*            BREAK BY estCostMaterial.formNo DESCENDING                                                 */
/*            BY estCostMaterial.blankNo                                                                 */
/*            BY estCostMaterial.itemID:                                                                 */
/*                                                                                                       */
/*            IF first-of(estCostMaterial.itemID) THEN                                                   */
/*                ASSIGN                                                                                 */
/*                    dActItemQty  = 0                                                                   */
/*                    dActItemCost = 0                                                                   */
/*                    dStdItemQty  = 0                                                                   */
/*                    dStdItemCost = 0.                                                                  */
/*                                                                                                       */
/*            RELEASE ttMaterial.                                                                        */
/*            FOR EACH mat-act NO-LOCK                                                                   */
/*                WHERE mat-act.company EQ ipbf-job.company                                              */
/*                AND mat-act.job-no EQ ipbf-job.job-no                                                  */
/*                AND mat-act.job-no EQ ipbf-job.job-no                                                  */
/*                AND mat-act.rm-i-no EQ estCostMaterial.itemID,                                         */
/*                FIRST ITEM NO-LOCK                                                                     */
/*                WHERE ITEM.company EQ mat-act.company                                                  */
/*                AND item.i-no  EQ mat-act.i-no:                                                        */
/*                                                                                                       */
/*                FIND FIRST ttMaterial NO-LOCK                                                          */
/*                    WHERE ttMaterial.cJobNo EQ ipbf-job.job-no                                         */
/*                    AND ttMaterial.iJobNo2 EQ ipbf-job.job-no2                                         */
/*                    AND ttMaterial.cMaterial EQ estCostMaterial.itemID                                 */
/*                    AND ttMaterial.iFormNo EQ mat-act.s-num                                            */
/*                    AND ttMaterial.iBlankNo EQ mat-act.b-num NO-ERROR.                                 */
/*                                                                                                       */
/*                IF NOT AVAILABLE ttMaterial THEN                                                       */
/*                DO:                                                                                    */
/*                    CREATE ttMaterial.                                                                 */
/*                    ASSIGN                                                                             */
/*                        ttMaterial.cJobNo    = ipbf-job.job-no                                         */
/*                        ttMaterial.iJobNo2   = ipbf-job.job-no2                                        */
/*                        ttMaterial.iFormNo   = mat-act.s-num                                           */
/*                        ttMaterial.iBlankNo  = mat-act.b-num                                           */
/*                        ttMaterial.cMaterial = estCostMaterial.itemID.                                 */
/*                END.                                                                                   */
/*                ASSIGN                                                                                 */
/*                    ttMaterial.dItemQty       = ttMaterial.dItemQty + mat-act.qty                      */
/*                    ttMaterial.dItemCost      = ttMaterial.dItemCost + mat-act.cost                    */
/*                    dActItemQty               = dActItemQty + mat-act.qty                              */
/*                    dActItemCost              = dActItemCost + mat-act.cost                            */
/*                    ttJob.dTotActMaterialCost = ttJob.dTotActMaterialCost + ttOperation.dCost          */
/*                    ttJob.dTotActCost         = ttJob.dTotActCost + ttOperation.dCost.                 */
/*            END.                                                                                       */
/*                                                                                                       */
/*            FIND FIRST ttMaterial NO-LOCK                                                              */
/*                WHERE ttMaterial.cJobNo EQ ipbf-job.job-no                                             */
/*                AND ttMaterial.iJobNo2 EQ ipbf-job.job-no2                                             */
/*                AND ttMaterial.cMaterial EQ estCostMaterial.itemID                                     */
/*                AND ttMaterial.iFormNo EQ estCostMaterial.formNo                                       */
/*                AND ttMaterial.iBlankNo EQ estCostMaterial.blankNo NO-ERROR.                           */
/*            IF NOT AVAILABLE ttMaterial THEN                                                           */
/*            DO:                                                                                        */
/*                CREATE ttMaterial.                                                                     */
/*                ASSIGN                                                                                 */
/*                    ttMaterial.cJobNo    = ipbf-job.job-no                                             */
/*                    ttMaterial.iJobNo2   = ipbf-job.job-no2                                            */
/*                    ttMaterial.cMaterial = estCostMaterial.itemID                                      */
/*                    ttMaterial.iFormNo   = estCostMaterial.formNo                                      */
/*                    ttMaterial.iBlankNo  = estCostMaterial.blankNo.                                    */
/*            END.                                                                                       */
/*            ASSIGN                                                                                     */
/*                ttMaterial.dItemQty       = ttMaterial.dItemQty + estCostMaterial.quantityRequiredTotal*/
/*                ttMaterial.dItemCost      = ttMaterial.dItemCost + estCostMaterial.costTotal           */
/*                dStdItemQty               = dStdItemQty + estCostMaterial.quantityRequiredTotal        */
/*                dStdItemCost              = dStdItemCost + estCostMaterial.costTotal                   */
/*                ttJob.dTotStdMaterialCost = ttJob.dTotStdMaterialCost + estCostMaterial.costTotal      */
/*                ttJob.dTotStdCost         = ttJob.dTotStdCost + estCostMaterial.costTotal.             */
/*                                                                                                       */
/*            IF LAST-OF(estCostMaterial.itemID) THEN                                                    */
/*            DO:                                                                                        */
/*                ASSIGN                                                                                 */
/*                    ttMaterial.dItemQtyVar  = (dStdItemQty - dActItemQty) / dStdItemQty * 100          */
/*                    ttMaterial.dItemCostVar = (dStdItemCost - dActItemCost) / dStdItemCost * 100 .     */
/*                                                                                                       */
/*                IF ttMaterial.dItemQtyVar EQ ? THEN ttMaterial.dItemQtyVar = 0.                        */
/*                IF ttMaterial.dItemCostVar EQ ? THEN ttMaterial.dItemCostVar = 0.                      */
/*            END.                                                                                       */
/*        END.                                                                                           */
/*    END.                                                                                               */
/*                                                                                                       */
/*                                                                                                       */
/*END PROCEDURE.                                                                                         */


/* ************************  Function Implementations ***************** */

FUNCTION fGetOnHandQty RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER, ipcJobNo AS CHARACTER, ipiJobNo2 AS INTEGER, ipcFGItem AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iRtnValue AS INTEGER NO-UNDO.
    FOR EACH fg-bin FIELDS(qty) NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.job-no EQ ipcJobNo
        AND fg-bin.job-no2 EQ ipiJobNo2
        AND fg-bin.i-no EQ ipcFGItem:
        iRtnValue = iRtnValue + fg-bin.qty.
    END. /* each fg-bin */
    RETURN iRtnValue.
END FUNCTION.
