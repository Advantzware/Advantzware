
/*------------------------------------------------------------------------
    File        : JobReport.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Sewa Singh
    Created     : Thur Jan 06 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{jc\ttJobReport.i}

DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcJobNo2 AS CHARACTER NO-UNDO.  
DEFINE OUTPUT PARAMETER TABLE FOR ttJobReport.

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

RUN pBuildTempTable(ipcJobNo, ipcJobNo2).

/* **********************  Internal Procedures  *********************** */
PROCEDURE pBuildTempTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo2 AS CHARACTER  NO-UNDO.
    
    FOR EACH job NO-LOCK 
        WHERE job.company EQ cCompany        
        AND job.job-no  ge SUBSTR(ipcJobNo,1,6)
        AND job.job-no  le SUBSTR(ipcJobNo2,1,6)
        AND fill(" ",6 - length(trim(job.job-no))) +
              trim(job.job-no) + string(int(job.job-no2),"99") GE ipcJobNo
        AND fill(" ",6 - length(trim(job.job-no))) +
              trim(job.job-no) + string(int(job.job-no2),"99") LE ipcJobNo2
        BY job.job-no
        BY job.job-no2:  
                
        IF AVAILABLE job THEN
        DO:
            CREATE ttJobReport.
            ASSIGN
                ttJobReport.typeItem  = "Header"
                ttJobReport.JobNo     = job.job-no
                ttJobReport.JobNo2    = job.job-no2
                ttJobReport.job       = job.job-no + "-" + STRING(job.job-no2)
                ttJobReport.closeDate = job.close-date.  
                     
            RUN pPrintFGItemInfo( BUFFER job).
            RUN pMachineOperations(BUFFER job).
            RUN pMaterial(BUFFER job).
            RUN pTotalCalcCost(BUFFER job).
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
        CREATE ttJobReport.
        ASSIGN
            ttJobReport.typeItem       = "FGItem"
            ttJobReport.JobNo          = ipbf-job.job-no
            ttJobReport.JobNo2         = ipbf-job.job-no2
            ttJobReport.job            = ipbf-job.job-no + "-" + STRING(job.job-no2)
            ttJobReport.closeDate      = ipbf-job.close-date  
            ttJobReport.formNo         = job-hdr.frm
            ttJobReport.blankNo        = job-hdr.blank-no
            ttJobReport.fgItem         = job-hdr.i-no
            ttJobReport.fgName         = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
            ttJobReport.fgSellingPrice = IF AVAILABLE itemfg THEN itemfg.sell-price ELSE 0
            ttJobReport.fgJobQty       = job-hdr.qty .
         
        RUN fg/GetProductionQty.p (INPUT job-hdr.company,
            INPUT job-hdr.job-no,
            INPUT job-hdr.job-no2,
            INPUT job-hdr.i-no,
            INPUT NO,
            OUTPUT iProductionQty).
                                
        ttJobReport.fgProduced = iProductionQty.
         
        ttJobReport.fgOnHand   = fGetOnHandQty(job-hdr.job-no,job-hdr.job-no2,job-hdr.i-no).
        . 
        
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ cCompany
             AND cust.cust-no EQ job-hdr.cust-no NO-ERROR.
        IF AVAILABLE cust THEN
        ttJobReport.custName       = cust.NAME.
    END.
END PROCEDURE.

PROCEDURE pMachineOperations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
     
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
            CREATE ttJobReport.
            ASSIGN
                ttJobReport.typeItem      = "Machine"
                ttJobReport.JobNo         = ipbf-job.job-no
                ttJobReport.JobNo2        = ipbf-job.job-no2
                ttJobReport.job           = ipbf-job.job-no + "-" + STRING(job.job-no2)
                ttJobReport.closeDate     = ipbf-job.close-date                     
                ttJobReport.formNo        = estCostOperation.formNo
                ttJobReport.blankNo       = estCostOperation.blankNo
                ttJobReport.mDept         = estCostOperation.departmentIDPrimary
                ttJobReport.mMachine      = estCostOperation.operationID
                ttJobReport.mStdAct       = "Standard"
                ttJobReport.mRunQty       = 0                     
                ttJobReport.mSetupHrs     = estCostOperation.hoursSetup                   
                ttJobReport.mRunHrs       = estCostOperation.hoursRun                  
                ttJobReport.mSpeed        = estCostOperation.speed
                ttJobReport.mCost         = estCostOperation.hoursRun * estCostOperation.costPerHourTotalRun
                ttJobReport.mSetupWaste   = estCostOperation.quantityInSetupWaste                 
                ttJobReport.mRunWaste     = estCostOperation.quantityInRunWaste                   
                ttJobReport.mDownTimeCode = ""
                ttJobReport.mDownTimeHrs  = estCostOperation.hoursSetup + estCostOperation.hoursRun.
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
            
            
        CREATE ttJobReport.
        ASSIGN
            ttJobReport.typeItem      = "Machine"
            ttJobReport.JobNo         = ipbf-job.job-no
            ttJobReport.JobNo2        = ipbf-job.job-no2
            ttJobReport.job           = ipbf-job.job-no + "-" + STRING(job.job-no2)
            ttJobReport.closeDate     = ipbf-job.close-date                     
            ttJobReport.formNo        = mch-act.frm
            ttJobReport.blankNo       = IF mach.p-type EQ "B"    AND
                                               mch-act.blank-no EQ 0 THEN 1
                                                                      ELSE mch-act.blank-no
            ttJobReport.mDept         = mch-act.dept
            ttJobReport.mMachine      = mch-act.m-code
            ttJobReport.mStdAct       = "Actual"
            ttJobReport.mRunQty       = mch-act.qty                     
            ttJobReport.mSetupHrs     = mch-act.hours                   
            ttJobReport.mRunHrs       = mch-act.hours                  
            ttJobReport.mSpeed        = mch-act.qty / mch-act.hours
            ttJobReport.mSetupWaste   = mch-act.waste                 
            ttJobReport.mRunWaste     = mch-act.waste                   
            ttJobReport.mDownTimeCode = mch-act.d-type
            ttJobReport.mDownTimeHrs  = mch-act.hours
            ttJobReport.mCost         = (mch-act.hours *  mach.mr-rate).    
                
        IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN 
        DO:
            ASSIGN
                ttJobReport.mRunQty       = mch-act.qty                     
                ttJobReport.mSetupHrs     = mch-act.hours                  
                ttJobReport.mRunHrs       = mch-act.hours                                                       
                ttJobReport.mDownTimeHrs  = mch-act.hours .    
                
        END.
        ELSE
            IF job-code.cat EQ "MR" THEN 
            DO:               
                ASSIGN                
                    ttJobReport.mRunQty = mch-act.qty + mch-act.waste
                    ttJobReport.mRunHrs = mch-act.hours                       
                    .
            END. /* else if job-code... */
    END.      
     
   
END PROCEDURE.

PROCEDURE pMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
     
    FIND FIRST estCostHeader NO-LOCK
        WHERE estCostHeader.company EQ cCompany
        AND estCostHeader.jobID EQ ipbf-job.job-no
        AND estCostHeader.jobID2 EQ ipbf-job.job-no2
        NO-ERROR.
         
    IF AVAILABLE estCostHeader THEN
    DO:
        FOR EACH estCostMaterial NO-LOCK 
            WHERE estCostMaterial.estCostHeaderID EQ estCostHeader.estCostHeaderID             
            BY estCostMaterial.formNo DESCENDING
            BY estCostMaterial.blankNo
            BY estCostMaterial.sequenceOfMaterial:
            
            CREATE ttJobReport.
            ASSIGN
                ttJobReport.typeItem  = "Material"
                ttJobReport.JobNo     = ipbf-job.job-no
                ttJobReport.JobNo2    = ipbf-job.job-no2
                ttJobReport.job       = ipbf-job.job-no + "-" + STRING(job.job-no2)
                ttJobReport.closeDate = ipbf-job.close-date                     
                ttJobReport.formNo    = estCostMaterial.formNo
                ttJobReport.blankNo   = estCostMaterial.blankNo
                ttJobReport.mStdAct   = "Standard"
                ttJobReport.itemCode  = estCostMaterial.itemID                    
                ttJobReport.itemQty   = estCostMaterial.quantityRequiredTotal                   
                ttJobReport.itemCost  = estCostMaterial.costTotal   .                
        END.           
    END.
     
    FOR EACH mat-act NO-LOCK
        WHERE mat-act.company EQ cCompany
        AND mat-act.job-no EQ ipbf-job.job-no
        AND mat-act.job-no EQ ipbf-job.job-no,
        FIRST ITEM NO-LOCK 
        WHERE ITEM.company EQ cCompany 
        AND mach.m-code  EQ mch-act.m-code:
           
                   
        CREATE ttJobReport.
        ASSIGN
            ttJobReport.typeItem  = "Material"
            ttJobReport.JobNo     = ipbf-job.job-no
            ttJobReport.JobNo2    = ipbf-job.job-no2
            ttJobReport.job       = ipbf-job.job-no + "-" + STRING(job.job-no2)
            ttJobReport.closeDate = ipbf-job.close-date                     
            ttJobReport.formNo    = mat-act.s-num
            ttJobReport.blankNo   = mat-act.b-num
            ttJobReport.mStdAct   = "Actual"
            ttJobReport.itemCode  = mat-act.rm-i-no
            ttJobReport.itemQty   = mat-act.qty
            ttJobReport.itemCost  = mat-act.cost
            .  
    END.      
     
   
END PROCEDURE.

PROCEDURE pTotalCalcCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
    DEFINE BUFFER bf-ttJobReport FOR ttJobReport.
    
    DEFINE VARIABLE dActMaterialQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdMaterialQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActMaterialCost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdMaterialCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dStdMachQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdSetupHrs AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdRunHrs AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdSpeed AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdSetupWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdRunWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStdCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dActMachQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActSetupHrs AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActRunHrs AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActSpeed AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActSetupWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActRunWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dActCost AS DECIMAL NO-UNDO.
    
    FIND FIRST ttJobReport NO-LOCK
        WHERE ttJobReport.typeItem EQ "Header" 
        AND ttJobReport.JobNo EQ ipbf-job.job-no
        AND ttJobReport.JobNo2 EQ ipbf-job.job-no2 NO-ERROR.
    IF AVAILABLE ttJobReport THEN
    DO:
        FOR EACH bf-ttJobReport NO-LOCK
            WHERE bf-ttJobReport.typeItem NE "Header"
            AND bf-ttJobReport.JobNo EQ ipbf-job.job-no
            AND bf-ttJobReport.JobNo2 EQ ipbf-job.job-no2
            BREAK BY bf-ttJobReport.typeItem
                  BY bf-ttJobReport.mStdAct:
                  
            IF bf-ttJobReport.typeItem EQ "FGItem" THEN       
            ttJobReport.custName =  bf-ttJobReport.custName.
            
            IF bf-ttJobReport.typeItem EQ "Material" THEN 
            DO:
                IF bf-ttJobReport.mStdAct EQ "Actual" THEN
                    ASSIGN
                    dActMaterialCost = dActMaterialCost + bf-ttJobReport.itemCost
                    dActMaterialQty = dActMaterialQty + bf-ttJobReport.itemQty 
                    ttJobReport.totActMaterialCost = ttJobReport.totActMaterialCost + bf-ttJobReport.itemCost.
                ELSE
                   ASSIGN
                    dStdMaterialCost = dStdMaterialCost + bf-ttJobReport.itemCost
                    dStdMaterialQty  = dActMaterialQty +  bf-ttJobReport.itemQty 
                    ttJobReport.totStdMaterialCost = ttJobReport.totStdMaterialCost + bf-ttJobReport.itemCost.
             
            END.
            ELSE IF bf-ttJobReport.typeItem EQ "Machine" THEN 
            DO:
                IF bf-ttJobReport.mStdAct EQ "Actual" THEN
                   ASSIGN
                      dActCost       = dActCost + bf-ttJobReport.mCost
                      dActMachQty    = dActMachQty + bf-ttJobReport.mRunQty
                      dActSetupHrs   = dActSetupHrs + bf-ttJobReport.mSetupHrs
                      dActRunHrs     = dActRunHrs + bf-ttJobReport.mRunHrs
                      dActSpeed      = dActSpeed + bf-ttJobReport.mSpeed
                      dActSetupWaste = dActSetupWaste + bf-ttJobReport.mSetupWaste
                      dActRunWaste   = dActRunWaste +  bf-ttJobReport.mRunWaste
                      ttJobReport.totActMachineCost = ttJobReport.totActMachineCost + bf-ttJobReport.mCost
                      .
                ELSE 
                  ASSIGN
                    dStdCost       = dStdCost + bf-ttJobReport.mCost
                    dStdMachQty    = dStdMachQty + bf-ttJobReport.mRunQty
                    dStdSetupHrs   = dStdSetupHrs + bf-ttJobReport.mSetupHrs
                    dStdRunHrs     = dStdRunHrs + bf-ttJobReport.mRunHrs
                    dStdSpeed      = dStdSpeed + bf-ttJobReport.mSpeed
                    dStdSetupWaste = dStdSetupWaste + bf-ttJobReport.mSetupWaste
                    dStdRunWaste   = dStdRunWaste +  bf-ttJobReport.mRunWaste
                    ttJobReport.totStdMachineCost = ttJobReport.totStdMachineCost + bf-ttJobReport.mCost.
            END.
            IF LAST(bf-ttJobReport.mStdAct) THEN
            DO:                        
                ASSIGN 
                     ttJobReport.mCost   = dStdCost / dActCost * 100
                     ttJobReport.itemQty = dStdMaterialQty / dActMaterialQty * 100
                     ttJobReport.mRunQty = dStdMachQty / dActMachQty * 100
                     ttJobReport.mSetupHrs = dStdSetupHrs / dActSetupHrs * 100
                     ttJobReport.mRunHrs = dStdRunHrs / dActRunHrs * 100
                     ttJobReport.mSpeed = dStdSpeed / dActSpeed * 100
                     ttJobReport.mSetupWaste = dStdSetupWaste / dActSetupWaste * 100
                     ttJobReport.mRunWaste = dStdRunWaste / dActRunWaste * 100                      
                     ttJobReport.itemCost = dStdMaterialCost / dActMaterialCost * 100
                     ttJobReport.itemQty = dStdMaterialQty / dActMaterialQty * 100  .
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
