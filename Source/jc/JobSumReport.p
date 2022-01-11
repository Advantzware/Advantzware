
/*------------------------------------------------------------------------
    File        : JobSumReport.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Sewa Singh
    Created     : Thur Jan 06 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{jc\ttJobReport.i}

DEFINE INPUT PARAMETER ipcFromJobNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiFromJobNo2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipcToJobNo AS CHARACTER NO-UNDO.   
DEFINE INPUT PARAMETER ipiToJobNo2 AS INTEGER NO-UNDO.  
DEFINE OUTPUT PARAMETER TABLE FOR ttJob.
DEFINE OUTPUT PARAMETER TABLE FOR ttDepartment.
DEFINE OUTPUT PARAMETER TABLE FOR ttOperation.
DEFINE OUTPUT PARAMETER TABLE FOR ttMaterial.
DEFINE OUTPUT PARAMETER TABLE FOR ttItem.

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLoc     AS CHARACTER NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN spGetSessionParam("Location", OUTPUT cLoc). 

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */
FUNCTION fGetOnHandQty RETURNS INTEGER PRIVATE
    (ipcJobNo AS CHARACTER,
    ipiJobNo2 AS INTEGER,
    ipcFGItem AS CHARACTER) FORWARD.
    
/* ***************************  Main Block  *************************** */
          
RUN pBuildTempTable(ipcFromJobNo, ipiFromJobNo2, ipcToJobNo, ipiToJobNo2).

/* **********************  Internal Procedures  *********************** */
PROCEDURE pBuildTempTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFromJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFromJobNo2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToJobNo AS CHARACTER  NO-UNDO.    
    DEFINE INPUT PARAMETER ipiToJobNo2 AS INTEGER  NO-UNDO.
    
    DEFINE VARIABLE cJobNo LIKE job.job-no EXTENT 2 INIT [" ", "zzzzzz"] NO-UNDO.
    ASSIGN
    cJobNo[1] = FILL(" ",6 - length(TRIM(ipcFromJobNo))) +
                  trim(ipcFromJobNo) + string(int(ipiFromJobNo2),"99")
    cJobNo[2] = FILL(" ",6 - length(TRIM(ipcToJobNo)))   +
                  trim(ipcToJobNo)   + string(int(ipiToJobNo2),"99"). 
    
    FOR EACH job NO-LOCK 
        WHERE job.company EQ cCompany        
        AND job.job-no  ge SUBSTR(cJobNo[1],1,6)
        AND job.job-no  le SUBSTR(cJobNo[2],1,6)
        AND fill(" ",6 - length(trim(job.job-no))) +
              trim(job.job-no) + string(int(job.job-no2),"99") GE cJobNo[1]
        AND fill(" ",6 - length(trim(job.job-no))) +
              trim(job.job-no) + string(int(job.job-no2),"99") LE cJobNo[2]  
        BY job.job-no
        BY job.job-no2:  
                
        IF AVAILABLE job THEN
        DO:
            CREATE ttJob.
            ASSIGN                
                ttJob.cJobNo      = job.job-no
                ttJob.iJobNo2     = job.job-no2
                ttJob.dtCloseDate = job.close-date
                ttJob.cEstimate   = job.est-no.  
                     
            RUN pPrintFGItemInfo( BUFFER job).
            RUN pMachineOperations(BUFFER job).
            RUN pMaterial(BUFFER job).            
        END.
    END.
END PROCEDURE.

PROCEDURE pPrintFGItemInfo PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
     
    DEFINE VARIABLE iProductionQty AS INTEGER NO-UNDO.
   
    FOR EACH job-hdr NO-LOCK 
        WHERE job-hdr.company EQ ipbf-job.company
        AND job-hdr.job-no EQ ipbf-job.job-no
        AND job-hdr.job-no2 EQ ipbf-job.job-no2
        AND job-hdr.job EQ ipbf-job.job
        BREAK BY job-hdr.frm
        BY job-hdr.blank-no :
              
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ ipbf-job.company
            AND itemfg.i-no EQ job-hdr.i-no 
            NO-ERROR. 
        CREATE ttItem.
        ASSIGN            
            ttItem.cJobNo          = ipbf-job.job-no
            ttItem.iJobNo2         = ipbf-job.job-no2               
            ttItem.iFormNo         = job-hdr.frm
            ttItem.iBlankNo        = job-hdr.blank-no
            ttItem.cFGItem         = job-hdr.i-no
            ttItem.cFGName         = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
            ttItem.cSellingPrice = IF AVAILABLE itemfg THEN itemfg.sell-price ELSE 0
            ttItem.dJobQty       = job-hdr.qty .
         
        RUN fg/GetProductionQty.p (INPUT job-hdr.company,
            INPUT job-hdr.job-no,
            INPUT job-hdr.job-no2,
            INPUT job-hdr.i-no,
            INPUT NO,
            OUTPUT iProductionQty).
                                
        ttItem.dProduced = iProductionQty.
         
        ttItem.dOnHand   = fGetOnHandQty(job-hdr.job-no,job-hdr.job-no2,job-hdr.i-no).
                
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ cCompany
             AND cust.cust-no EQ job-hdr.cust-no NO-ERROR.
        IF AVAILABLE cust THEN
        ttJob.cCustName       = cust.NAME.
    END.
    
    FIND FIRST estCostHeader NO-LOCK
         WHERE estCostHeader.company EQ cCompany
         AND estCostHeader.jobID EQ ipbf-job.job-no
         AND estCostHeader.jobID2 EQ ipbf-job.job-no2
        NO-ERROR.
        
    FOR EACH  estCostItem NO-LOCK 
        WHERE estCostItem.estCostHeaderID EQ estCostHeader.estCostHeaderID:
        
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipbf-job.company
             AND itemfg.i-no EQ estCostItem.itemID 
            NO-ERROR. 
        FIND FIRST ttItem NO-LOCK
             WHERE ttItem.cJobNo EQ ipbf-job.job-no
             AND ttItem.iJobNo2   EQ ipbf-job.job-no2
             AND ttItem.cFGItem  EQ  estCostItem.itemID  NO-ERROR.
        IF NOT AVAILABLE ttItem THEN
        DO:
             CREATE ttItem.
                ASSIGN            
                    ttItem.cJobNo          = ipbf-job.job-no
                    ttItem.iJobNo2         = ipbf-job.job-no2               
                    ttItem.iFormNo         = estCostItem.formNo
                    ttItem.iBlankNo        = estCostItem.blankNo
                    ttItem.cFGItem         = estCostItem.itemID
                    ttItem.cFGName         = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
                    ttItem.cSellingPrice = IF AVAILABLE itemfg THEN itemfg.sell-price ELSE 0
                    ttItem.dJobQty       = job-hdr.qty .
                 
                RUN fg/GetProductionQty.p (INPUT ipbf-job.company,
                    INPUT ipbf-job.job-no,
                    INPUT ipbf-job.job-no2,
                    INPUT estCostItem.itemID,
                    INPUT NO,
                    OUTPUT iProductionQty).
                                        
                ttItem.dProduced = iProductionQty.
                 
                ttItem.dOnHand   = fGetOnHandQty(ipbf-job.job-no,ipbf-job.job-no2,estCostItem.itemID).
        END.
            
        
    END.
    
END PROCEDURE.

PROCEDURE pMachineOperations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
    
    DEFINE VARIABLE dStdRunQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdSetupHrs AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdRunHrs AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdSpeed AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdSetupWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdRunWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dActRunQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActSetupHrs AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActRunHrs AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActSpeed AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActSetupWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActRunWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActCost AS DECIMAL NO-UNDO.
     
    FIND FIRST estCostHeader NO-LOCK
        WHERE estCostHeader.company EQ cCompany
        AND estCostHeader.jobID EQ ipbf-job.job-no
        AND estCostHeader.jobID2 EQ ipbf-job.job-no2
        NO-ERROR.
         
    IF AVAILABLE estCostHeader THEN
    DO:
        FOR EACH estCostOperation NO-LOCK 
            WHERE estCostOperation.estCostHeaderID EQ estCostHeader.estCostHeaderID
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
                ttOperation.dCost         = estCostOperation.hoursRun * estCostOperation.costPerHourTotalRun
                ttOperation.dSetupWaste   = estCostOperation.quantityInSetupWaste                 
                ttOperation.dRunWaste     = estCostOperation.quantityInRunWaste                   
                ttOperation.cDownTimeCode = ""
                ttOperation.dDownTimeHrs  = estCostOperation.hoursSetup + estCostOperation.hoursRun.
        END.           
    END.
     
    FOR EACH mch-act NO-LOCK
        WHERE mch-act.company EQ cCompany
        AND mch-act.job-no EQ ipbf-job.job-no
        AND mch-act.job-no EQ ipbf-job.job-no,
        FIRST mach WHERE
        mach.company EQ cCompany AND
        mach.loc     EQ cLoc AND
        mach.m-code  EQ mch-act.m-code
        NO-LOCK:
           
        FIND FIRST job-code NO-LOCK
            WHERE job-code.code EQ mch-act.code
            NO-ERROR.
            
            
        CREATE ttOperation.
        ASSIGN
            ttOperation.cJobNo        = ipbf-job.job-no
            ttOperation.iJobNo2       = ipbf-job.job-no2                                              
            ttOperation.iFormNo        = mch-act.frm
            ttOperation.iBlankNo       = IF mach.p-type EQ "B"    AND
                                               mch-act.blank-no EQ 0 THEN 1
                                                                      ELSE mch-act.blank-no
            ttOperation.cDept         = mch-act.dept
            ttOperation.cMachine      = mch-act.m-code
            ttOperation.cStdAct       = "Actual"
            ttOperation.dRunQty       = mch-act.qty                     
            ttOperation.dSetupHrs     = mch-act.hours                   
            ttOperation.dRunHrs       = mch-act.hours                  
            ttOperation.dSpeed        = mch-act.qty / mch-act.hours
            ttOperation.dSetupWaste   = mch-act.waste                 
            ttOperation.dRunWaste     = mch-act.waste                   
            ttOperation.cDownTimeCode = mch-act.d-type
            ttOperation.dDownTimeHrs  = mch-act.hours
            ttOperation.dCost         = (mch-act.hours *  mach.mr-rate).    
                
        IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN 
        DO:
            ASSIGN
                ttOperation.dRunQty       = mch-act.qty                     
                ttOperation.dSetupHrs     = mch-act.hours                  
                ttOperation.dRunHrs       = mch-act.hours                                                       
                ttOperation.dDownTimeHrs  = mch-act.hours .    
                
        END.
        ELSE
            IF job-code.cat EQ "MR" THEN 
            DO:               
                ASSIGN                
                    ttOperation.dRunQty = mch-act.qty + mch-act.waste
                    ttOperation.dRunHrs = mch-act.hours                       
                    .
            END. /* else if job-code... */
    END.  
    
    FOR EACH ttOperation NO-LOCK
        WHERE ttOperation.cJobNo EQ ipbf-job.job-no
        AND ttOperation.iJobNo2 EQ ipbf-job.job-no2
         BREAK BY ttOperation.cJobNo
               BY ttOperation.iJobNo2
               BY ttOperation.cDept:
               
        IF FIRST-OF(ttOperation.cDept) THEN
        ASSIGN
             dActRunQty       = 0            
             dActSetupHrs     = 0                   
             dActRunHrs       = 0                  
             dActSpeed        = 0
             dActSetupWaste   = 0                 
             dActRunWaste     = 0 
             dActCost         = 0
             dStdRunQty       = 0            
             dStdSetupHrs     = 0                   
             dStdRunHrs       = 0                  
             dStdSpeed        = 0
             dStdSetupWaste   = 0                 
             dStdRunWaste     = 0  
             dStdCost         = 0.
        
        
        FIND FIRST ttDepartment NO-LOCK
             WHERE ttDepartment.cJobNo EQ ipbf-job.job-no
             AND ttDepartment.iJobNo2 EQ ipbf-job.job-no2
             AND ttDepartment.cDept EQ ttOperation.cDept NO-ERROR.
        IF NOT AVAIL ttDepartment THEN
        DO:
           CREATE ttDepartment.
           ASSIGN
                ttDepartment.cJobNo        = ipbf-job.job-no
                ttDepartment.iJobNo2       = ipbf-job.job-no2
                ttDepartment.cDept         = ttOperation.cDept.            
        END.          
        ASSIGN            
            ttDepartment.dRunQty       = ttDepartment.dRunQty + ttOperation.dRunQty            
            ttDepartment.dSetupHrs     = ttDepartment.dSetupHrs + ttOperation.dSetupHrs                   
            ttDepartment.dRunHrs       = ttDepartment.dRunHrs + ttOperation.dRunHrs                  
            ttDepartment.dSpeed        = ttDepartment.dSpeed + ttOperation.dSpeed
            ttDepartment.dSetupWaste   = ttDepartment.dSetupWaste + ttOperation.dSetupWaste                 
            ttDepartment.dRunWaste     = ttDepartment.dRunWaste + ttOperation.dRunWaste  
            ttDepartment.dCost         = ttDepartment.dCost + ttOperation.dCost
            ttDepartment.cDownTimeCode = ttOperation.cDownTimeCode
            ttDepartment.dDownTimeHrs  = ttDepartment.dDownTimeHrs + ttOperation.dDownTimeHrs
            .        
        
        IF ttOperation.cStdAct EQ "Actual" THEN
        assign
             ttJob.dTotActMachineCost = ttJob.dTotActMachineCost + ttOperation.dCost
             ttJob.dTotActCost = ttJob.dTotActCost + ttOperation.dCost                     
             dActRunQty       = dActRunQty + ttOperation.dRunQty            
             dActSetupHrs     = dActSetupHrs + ttOperation.dSetupHrs                   
             dActRunHrs       = dActRunHrs + ttOperation.dRunHrs                  
             dActSpeed        = dActSpeed + ttOperation.dSpeed
             dActSetupWaste   = dActSetupWaste + ttOperation.dSetupWaste                 
             dActRunWaste     = dActRunWaste + ttOperation.dRunWaste  
             dActCost         = dActCost + ttOperation.dCost
             .
        IF ttOperation.cStdAct EQ "Standard" THEN 
        assign
             ttJob.dTotStdMachineCost = ttJob.dTotStdMachineCost + ttOperation.dCost
             ttJob.dTotStdCost = ttJob.dTotStdCost + ttOperation.dCost
             dStdRunQty       = dStdRunQty + ttOperation.dRunQty            
             dStdSetupHrs     = dStdSetupHrs + ttOperation.dSetupHrs                   
             dStdRunHrs       = dStdRunHrs + ttOperation.dRunHrs                  
             dStdSpeed        = dStdSpeed + ttOperation.dSpeed
             dStdSetupWaste   = dStdSetupWaste + ttOperation.dSetupWaste                 
             dStdRunWaste     = dStdRunWaste + ttOperation.dRunWaste  
             dStdCost         = dStdCost + ttOperation.dCost
             .
             
        IF LAST-OF(ttOperation.cDept) THEN
        DO:
           ASSIGN
               ttDepartment.dRunQtyVar       = (dStdRunQty - dActRunQty) / dStdRunQty * 100 
               ttDepartment.dSetupHrsVar     = (dStdSetupHrs - dActSetupHrs) / dStdSetupHrs * 100                   
               ttDepartment.dRunHrsVar       = (dStdRunHrs - dActRunHrs) / dStdRunHrs * 100                   
               ttDepartment.dSpeedVar        = (dStdSpeed - dActSpeed) / dStdSpeed * 100 
               ttDepartment.dSetupWasteVar   = (dStdSetupWaste - dActSetupWaste) / dStdSetupWaste * 100                  
               ttDepartment.dRunWasteVar     = (dStdRunWaste - dActRunWaste) / dStdRunWaste * 100   
               ttDepartment.dCostVar         = (dStdCost - dActCost) / dStdCost * 100 
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

PROCEDURE pMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
    
    DEFINE VARIABLE dStdItemQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdItemCost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActItemQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActItemCost AS DECIMAL NO-UNDO.
     
    FIND FIRST estCostHeader NO-LOCK
        WHERE estCostHeader.company EQ cCompany
        AND estCostHeader.jobID EQ ipbf-job.job-no
        AND estCostHeader.jobID2 EQ ipbf-job.job-no2
        NO-ERROR.
         
    IF AVAILABLE estCostHeader THEN
    DO:
        FOR EACH estCostMaterial NO-LOCK 
            WHERE estCostMaterial.estCostHeaderID EQ estCostHeader.estCostHeaderID             
            BREAK BY estCostMaterial.formNo DESCENDING
            BY estCostMaterial.blankNo
            BY estCostMaterial.itemID:
            
            IF first-of(estCostMaterial.itemID) THEN
            ASSIGN
                 dActItemQty  = 0
                 dActItemCost = 0
                 dStdItemQty  = 0
                 dStdItemCost = 0.                 
            
            RELEASE ttMaterial.
            FOR EACH mat-act NO-LOCK
                WHERE mat-act.company EQ cCompany
                AND mat-act.job-no EQ ipbf-job.job-no
                AND mat-act.job-no EQ ipbf-job.job-no
                AND mat-act.rm-i-no EQ estCostMaterial.itemID,
                FIRST ITEM NO-LOCK 
                WHERE ITEM.company EQ cCompany 
                AND mach.m-code  EQ mch-act.m-code:
                
                FIND FIRST ttMaterial NO-LOCK
                     WHERE ttMaterial.cJobNo EQ ipbf-job.job-no
                     AND ttMaterial.iJobNo2 EQ ipbf-job.job-no2
                     AND ttMaterial.cMaterial EQ estCostMaterial.itemID
                     AND ttMaterial.iFormNo EQ mat-act.s-num
                     AND ttMaterial.iBlankNo EQ mat-act.b-num NO-ERROR.
                
                IF NOT AVAILABLE ttMaterial THEN
                DO:
                    CREATE ttMaterial.
                    ASSIGN
                         ttMaterial.cJobNo     = ipbf-job.job-no
                         ttMaterial.iJobNo2    = ipbf-job.job-no2 
                         ttMaterial.iFormNo     = mat-act.s-num
                         ttMaterial.iBlankNo    = mat-act.b-num                             
                         ttMaterial.cMaterial  = estCostMaterial.itemID.
                END.                                          
                ASSIGN                                          
                    ttMaterial.dItemQty   = ttMaterial.dItemQty + mat-act.qty
                    ttMaterial.dItemCost  = ttMaterial.dItemCost + mat-act.cost   
                    dActItemQty           = dActItemQty + mat-act.qty
                    dActItemCost          = dActItemCost + mat-act.cost
                    ttJob.dTotActMaterialCost = ttJob.dTotActMaterialCost + ttOperation.dCost
                    ttJob.dTotActCost = ttJob.dTotActCost + ttOperation.dCost.                
            END.
            
            FIND FIRST ttMaterial NO-LOCK
                     WHERE ttMaterial.cJobNo EQ ipbf-job.job-no
                     AND ttMaterial.iJobNo2 EQ ipbf-job.job-no2
                     AND ttMaterial.cMaterial EQ estCostMaterial.itemID 
                     AND ttMaterial.iFormNo EQ estCostMaterial.formNo
                     AND ttMaterial.iBlankNo EQ estCostMaterial.blankNo NO-ERROR.
            IF NOT AVAILABLE ttMaterial THEN
            DO:
                CREATE ttMaterial.
                ASSIGN
                     ttMaterial.cJobNo     = ipbf-job.job-no
                     ttMaterial.iJobNo2    = ipbf-job.job-no2                       
                     ttMaterial.cMaterial  = estCostMaterial.itemID
                     ttMaterial.iFormNo    = estCostMaterial.formNo
                     ttMaterial.iBlankNo   = estCostMaterial.blankNo.
            END.
            ASSIGN
                 ttMaterial.dItemQty   = ttMaterial.dItemQty + estCostMaterial.quantityRequiredTotal
                 ttMaterial.dItemCost  = ttMaterial.dItemCost + estCostMaterial.costTotal 
                 dStdItemQty           = dStdItemQty + estCostMaterial.quantityRequiredTotal
                 dStdItemCost          = dStdItemCost + estCostMaterial.costTotal
                 ttJob.dTotStdMaterialCost = ttJob.dTotStdMaterialCost + estCostMaterial.costTotal
                 ttJob.dTotStdCost         = ttJob.dTotStdCost + estCostMaterial.costTotal.
                 
           IF LAST-OF(estCostMaterial.itemID) THEN
           DO:
             ASSIGN
                 ttMaterial.dItemQtyVar   = (dStdItemQty - dActItemQty) / dStdItemQty * 100
                 ttMaterial.dItemCostVar  = (dStdItemCost - dActItemCost) / dStdItemCost * 100 . 
                 
             IF ttMaterial.dItemQtyVar EQ ? THEN ttMaterial.dItemQtyVar = 0.    
             IF ttMaterial.dItemCostVar EQ ? THEN ttMaterial.dItemCostVar = 0.    
           END.      
        END.           
    END.
     
      
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetOnHandQty RETURNS INTEGER PRIVATE
    (ipcJobNo AS CHARACTER, ipiJobNo2 AS INTEGER, ipcFGItem AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iRtnValue AS INTEGER NO-UNDO.
    FOR EACH fg-bin FIELDS(qty) NO-LOCK
        WHERE fg-bin.company EQ cCompany
        AND fg-bin.job-no EQ ipcJobNo
        AND fg-bin.job-no2 EQ ipiJobNo2
        AND fg-bin.i-no EQ ipcFGItem:
        iRtnValue = iRtnValue + fg-bin.qty.
    END. /* each fg-bin */
    RETURN iRtnValue.
END FUNCTION.
