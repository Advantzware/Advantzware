
/*------------------------------------------------------------------------
    File        : BuildJob.p
    Purpose     : Replaces (some parts of) jc-calc.p

    Syntax      :

    Description : Builds all job Records for a given estimate

    Author(s)   : BV
    Created     : Wed Jun 03 15:47:32 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriJob AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iplNew AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
DEFINE TEMP-TABLE ttJobHdrToKeep
    FIELD riJobHdr AS ROWID.

DEFINE TEMP-TABLE ttJobMatToKeep
    FIELD riJobMat AS ROWID.

DEFINE TEMP-TABLE ttJobMchToKeep
    FIELD riJobMch AS ROWID.

DEFINE TEMP-TABLE ttJobPrepToKeep
    FIELD riJobPrep AS ROWID.
    
DEFINE TEMP-TABLE ttJobFarmToKeep
    FIELD riJobFarm AS ROWID.
    
/* ***************************  Main Block  *************************** */

RUN pBuildJob(ipriJob, iplNew, OUTPUT oplError, OUTPUT opcErrorMessage).

/* **********************  Internal Procedures  *********************** */
PROCEDURE pBuildFarm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an estimateCostHeaderID and job buffer, build the job-farm
        records (or update them)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-job    FOR job.
    
    DEFINE           BUFFER bf-job-farm FOR job-farm.
    
    FOR EACH estCostMaterial NO-LOCK
        WHERE estCostMaterial.estCostHeaderID EQ ipiEstCostHeaderID
        AND estCostMaterial.isPurchasedFG:
        FIND FIRST bf-job-farm EXCLUSIVE-LOCK 
            WHERE bf-job-farm.company EQ ipbf-job.company
            AND bf-job-farm.job EQ ipbf-job.job
            AND bf-job-farm.job-no EQ ipbf-job.job-no
            AND bf-job-farm.job-no2 EQ ipbf-job.job-no2
            AND bf-job-farm.frm EQ estCostMaterial.formNo
            AND bf-job-farm.blank-no EQ estCostMaterial.blankNo
            AND bf-job-farm.i-no EQ estCostMaterial.itemID
            NO-ERROR.
        IF NOT AVAILABLE bf-job-farm THEN 
        DO:
            CREATE bf-job-farm.
            ASSIGN 
                bf-job-farm.company  = ipbf-job.company
                bf-job-farm.job      = ipbf-job.job
                bf-job-farm.job-no   = ipbf-job.job-no
                bf-job-farm.job-no2  = ipbf-job.job-no2
                bf-job-farm.frm      = estCostMaterial.formNo
                bf-job-farm.blank-no = estCostMaterial.blankNo
                bf-job-farm.i-no     = estCostMaterial.itemID
                .
        END.
        CREATE ttJobFarmToKeep.
        ASSIGN 
            ttJobFarmToKeep.riJobFarm = ROWID(bf-job-farm)
            bf-job-farm.len           = estCostMaterial.dimLength
            bf-job-farm.wid           = estCostMaterial.dimWidth
            bf-job-farm.dep           = estCostMaterial.dimDepth
            bf-job-farm.qty           = estCostMaterial.quantityRequiredTotal
            bf-job-farm.qty-uom       = estCostMaterial.quantityUOM
            bf-job-farm.std-cost      = estCostMaterial.costPerUOM
            bf-job-farm.sc-uom        = estCostMaterial.costUOM
            bf-job-farm.cost-m        = estCostMaterial.costTotalPerMFinished
            .
            
    END.
    RELEASE bf-job-farm.
    
END PROCEDURE.

PROCEDURE pBuildJob PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Main Job Build private proc
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJob AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplNew AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job FOR job.
    
    DEFINE VARIABLE iEstCostHeaderID AS INT64 NO-UNDO.
    
    FIND FIRST bf-job NO-LOCK 
        WHERE ROWID(bf-job) EQ ipriJob
        NO-ERROR.
    IF AVAILABLE bf-job THEN 
    DO:
        RUN pCalcEstimateForJob(BUFFER bf-job, 0, OUTPUT iEstCostHeaderID).
        RUN pBuildHeaders(iEstCostHeaderID, BUFFER bf-job).
        RUN pBuildMaterials(iEstCostHeaderID, BUFFER bf-job).
        RUN pBuildMachines(iEstCostHeaderID, BUFFER bf-job).
        RUN pBuildMisc(iEstCostHeaderID, BUFFER bf-job).
        RUN pBuildFarm(iEstCostHeaderID, BUFFER bf-job).
        RUN pCleanJob(BUFFER bf-job).
    END.
    ELSE 
        ASSIGN 
            oplError        = YES
            opcErrorMessage = "Invalid Job Row ID"
            .

END PROCEDURE.

PROCEDURE pBuildHeaders PRIVATE:
    /*------------------------------------------------------------------------------
        Purpose:  Given an estimateCostHeaderID and job buffer, build the job-hdr
           records (or update them)
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-job    FOR job.
    
    DEFINE           BUFFER bf-job-hdr  FOR job-hdr.
    DEFINE           BUFFER bf-reftable FOR reftable.
    
    DEFINE VARIABLE dQtyInM AS DECIMAL NO-UNDO.
    
    FOR EACH bf-reftable
        WHERE bf-reftable.reftable EQ "jc/jc-calc.p"
        AND bf-reftable.company  EQ ipbf-job.company
        AND bf-reftable.loc      EQ ""
        AND bf-reftable.code     EQ STRING(ipbf-job.job,"999999999")
        TRANSACTION:
        DELETE bf-reftable.
    END.
    FOR EACH estCostBlank NO-LOCK
        WHERE estCostBlank.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST estCostItem NO-LOCK 
        WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
        AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID,
        FIRST estCostHeader NO-LOCK 
        WHERE estCostHeader.estCostHeaderID EQ estCostBlank.estCostHeaderID:
        FIND FIRST bf-job-hdr EXCLUSIVE-LOCK 
            WHERE bf-job-hdr.company EQ ipbf-job.company
            AND bf-job-hdr.job EQ ipbf-job.job
            AND bf-job-hdr.job-no EQ ipbf-job.job-no
            AND bf-job-hdr.job-no2 EQ ipbf-job.job-no2
            AND bf-job-hdr.frm EQ estCostBlank.formNo
            AND bf-job-hdr.blank-no EQ estCostBlank.blankNo
            AND bf-job-hdr.i-no EQ estCostItem.itemID
            NO-ERROR.
        IF NOT AVAILABLE bf-job-hdr THEN 
        DO:
            CREATE bf-job-hdr.
            ASSIGN 
                bf-job-hdr.company  = ipbf-job.company
                bf-job-hdr.job      = ipbf-job.job
                bf-job-hdr.job-no   = ipbf-job.job-no
                bf-job-hdr.job-no2  = ipbf-job.job-no2
                bf-job-hdr.frm      = estCostBlank.formNo
                bf-job-hdr.blank-no = estCostBlank.blankNo
                bf-job-hdr.i-no     = estCostItem.itemID
                bf-job-hdr.qty      = estCostBlank.quantityRequired
                bf-job-hdr.cust-no  = estCostItem.customerID
                bf-job-hdr.est-no   = ipbf-job.est-no
                .
        END.
        CREATE ttJobHdrToKeep.
        ASSIGN 
            dQtyInM                 = bf-job-hdr.qty / 1000
            ttJobHdrToKeep.riJobHdr = ROWID(bf-job-hdr)
            bf-job-hdr.std-tot-cost = estCostItem.costTotalFactory / dQtyInM
            bf-job-hdr.std-mat-cost = estCostItem.costTotalMaterial / dQtyInM
            bf-job-hdr.std-lab-cost = estCostItem.costTotalLabor / dQtyInM
            bf-job-hdr.std-var-cost = estCostItem.costTotalVariableOverhead / dQtyInM
            bf-job-hdr.std-fix-cost = estCostItem.costTotalFixedOverhead / dQtyInM
            .
        IF estCostHeader.estType EQ "Set" AND NOT estCostItem.isSet THEN 
        DO:
            FIND FIRST bf-reftable
                WHERE bf-reftable.reftable EQ "jc/jc-calc.p"
                AND bf-reftable.company  EQ ipbf-job.company
                AND bf-reftable.loc      EQ ""
                AND bf-reftable.code     EQ STRING(ipbf-job.job,"999999999")
                AND bf-reftable.code2    EQ estCostItem.itemID
                AND bf-reftable.val[12]  EQ estCostBlank.formNo
                AND bf-reftable.val[13]  EQ estCostBlank.blankNo
                USE-INDEX reftable NO-ERROR.
            IF NOT AVAILABLE bf-reftable THEN 
            DO:
                CREATE bf-reftable.
                ASSIGN
                    bf-reftable.reftable = "jc/jc-calc.p"
                    bf-reftable.company  = ipbf-job.company
                    bf-reftable.loc      = ""
                    bf-reftable.code     = STRING(ipbf-job.job,"999999999")
                    bf-reftable.code2    = estCostItem.itemID
                    bf-reftable.val[12]  = estCostBlank.formNo
                    bf-reftable.val[13]  = estCostBlank.blankNo
                    bf-reftable.val[11]  = estCostBlank.numOut
                    .
            END.
            ASSIGN
                bf-reftable.val[1] = bf-reftable.val[1] + bf-job-hdr.std-lab-cost
                bf-reftable.val[2] = bf-reftable.val[2] + bf-job-hdr.std-mat-cost
                bf-reftable.val[3] = bf-reftable.val[3] + bf-job-hdr.std-var-cost
                bf-reftable.val[4] = bf-reftable.val[4] + bf-job-hdr.std-fix-cost
                bf-reftable.val[5] = bf-reftable.val[1] + bf-reftable.val[2] +
                         bf-reftable.val[3] + bf-reftable.val[4].
        END.
    END.
    RELEASE bf-job-hdr.
    RELEASE bf-reftable.
    
END PROCEDURE.

PROCEDURE pBuildMachines PRIVATE:
    /*------------------------------------------------------------------------------
        Purpose:  Given an estimateCostHeaderID and job buffer, build the job-mch
           records (or update them)
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-job   FOR job.
    
    DEFINE           BUFFER bf-job-mch FOR job-mch.
    
    FOR EACH estCostOperation NO-LOCK
        WHERE estCostOperation.estCostHeaderID EQ ipiEstCostHeaderID:
        FIND FIRST bf-job-mch EXCLUSIVE-LOCK 
            WHERE bf-job-mch.company EQ ipbf-job.company
            AND bf-job-mch.job EQ ipbf-job.job
            AND bf-job-mch.job-no EQ ipbf-job.job-no
            AND bf-job-mch.job-no2 EQ ipbf-job.job-no2
            AND bf-job-mch.frm EQ estCostOperation.formNo
            AND bf-job-mch.blank-no EQ estCostOperation.blankNo
            AND bf-job-mch.m-code EQ estCostOperation.operationID
            NO-ERROR.
        IF NOT AVAILABLE bf-job-mch THEN 
        DO:
            CREATE bf-job-mch.
            ASSIGN 
                bf-job-mch.company  = ipbf-job.company
                bf-job-mch.job      = ipbf-job.job
                bf-job-mch.job-no   = ipbf-job.job-no
                bf-job-mch.job-no2  = ipbf-job.job-no2
                bf-job-mch.frm      = estCostOperation.formNo
                bf-job-mch.blank-no = estCostOperation.blankNo
                bf-job-mch.m-code   = estCostOperation.operationID
                .
        END.
        CREATE ttJobMchToKeep.
        ASSIGN 
            ttJobMchToKeep.riJobMch = ROWID(bf-job-mch)
            bf-job-mch.mr-fixoh     = estCostOperation.costPerHourFOSetup
            bf-job-mch.mr-varoh     = estCostOperation.costPerHourVOSetup
            bf-job-mch.mr-rate      = estCostOperation.costPerManHourDLSetup * estCostOperation.crewSizeSetup
            bf-job-mch.mr-trate     = estCostOperation.costPerHourTotalSetup
            bf-job-mch.mr-hr        = estCostOperation.hoursSetup
            bf-job-mch.run-fixoh    = estCostOperation.costPerHourFORun
            bf-job-mch.run-varoh    = estCostOperation.costPerHourVORun
            bf-job-mch.run-rate     = estCostOperation.costPerManHourDLRun * estCostOperation.crewSizeRun
            bf-job-mch.run-trate    = estCostOperation.costPerHourTotalRun
            bf-job-mch.run-hr       = estCostOperation.hoursRun
            bf-job-mch.speed        = estCostOperation.speed
            bf-job-mch.line         = estCostOperation.sequenceOfOperation
            bf-job-mch.dept         = estCostOperation.departmentIDPrimary
            bf-job-mch.wst-prct     = estCostOperation.quantityInRunWastePercent
            bf-job-mch.mr-waste     = estCostOperation.quantityInSetupWaste
            bf-job-mch.run-qty      = estCostOperation.quantityIn
            . 
            
    END.
    RELEASE bf-job-mch.
    
END PROCEDURE.

PROCEDURE pBuildMaterials PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an estimateCostHeaderID and job buffer, build the job-mat
        records (or update them)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-job   FOR job.
    
    DEFINE           BUFFER bf-job-mat FOR job-mat.
    
    FOR EACH estCostMaterial NO-LOCK
        WHERE estCostMaterial.estCostHeaderID EQ ipiEstCostHeaderID
        AND NOT estCostMaterial.isPurchasedFG:
        FIND FIRST bf-job-mat EXCLUSIVE-LOCK 
            WHERE bf-job-mat.company EQ ipbf-job.company
            AND bf-job-mat.job EQ ipbf-job.job
            AND bf-job-mat.job-no EQ ipbf-job.job-no
            AND bf-job-mat.job-no2 EQ ipbf-job.job-no2
            AND bf-job-mat.frm EQ estCostMaterial.formNo
            AND bf-job-mat.blank-no EQ estCostMaterial.blankNo
            AND bf-job-mat.i-no EQ estCostMaterial.itemID
            NO-ERROR.
        IF NOT AVAILABLE bf-job-mat THEN 
        DO:
            CREATE bf-job-mat.
            ASSIGN 
                bf-job-mat.company  = ipbf-job.company
                bf-job-mat.job      = ipbf-job.job
                bf-job-mat.job-no   = ipbf-job.job-no
                bf-job-mat.job-no2  = ipbf-job.job-no2
                bf-job-mat.frm      = estCostMaterial.formNo
                bf-job-mat.blank-no = estCostMaterial.blankNo
                bf-job-mat.i-no     = estCostMaterial.itemID
                bf-job-mat.rm-i-no  = estCostMaterial.itemID
                .
        END.
        CREATE ttJobMatToKeep.
        ASSIGN 
            ttJobMatToKeep.riJobMat = ROWID(bf-job-mat)
            bf-job-mat.len          = estCostMaterial.dimLength
            bf-job-mat.wid          = estCostMaterial.dimWidth
            bf-job-mat.dep          = estCostMaterial.dimDepth
            bf-job-mat.qty          = estCostMaterial.quantityRequiredTotal
            bf-job-mat.qty-uom      = estCostMaterial.quantityUOM
            bf-job-mat.std-cost     = estCostMaterial.costPerUOM
            bf-job-mat.sc-uom       = estCostMaterial.costUOM
            bf-job-mat.cost-m       = estCostMaterial.costTotalPerMFinished
            .
    END.
    FOR EACH estCostMisc NO-LOCK 
        WHERE estCostMisc.estCostHeaderID EQ ipiEstCostHeaderID
        AND LOOKUP(estCostMisc.simon,"S,N,O") EQ 0
        AND estCostMisc.costType EQ "Mat"
        AND estCostMisc.itemID NE "",
        FIRST ITEM NO-LOCK 
            WHERE ITEM.company EQ estCostMisc.company
            AND ITEM.i-no EQ estCostMisc.itemID:
        
        FIND FIRST bf-job-mat EXCLUSIVE-LOCK 
            WHERE bf-job-mat.company EQ ipbf-job.company
            AND bf-job-mat.job EQ ipbf-job.job
            AND bf-job-mat.job-no EQ ipbf-job.job-no
            AND bf-job-mat.job-no2 EQ ipbf-job.job-no2
            AND bf-job-mat.frm EQ estCostMisc.formNo
            AND bf-job-mat.blank-no EQ estCostMisc.blankNo
            AND bf-job-mat.i-no EQ estCostMisc.itemID
            NO-ERROR.
        IF NOT AVAILABLE bf-job-mat THEN 
        DO:
            CREATE bf-job-mat.
            ASSIGN 
                bf-job-mat.company  = ipbf-job.company
                bf-job-mat.job      = ipbf-job.job
                bf-job-mat.job-no   = ipbf-job.job-no
                bf-job-mat.job-no2  = ipbf-job.job-no2
                bf-job-mat.frm      = estCostMisc.formNo
                bf-job-mat.blank-no = estCostMisc.blankNo
                bf-job-mat.i-no     = estCostMisc.itemID
                bf-job-mat.rm-i-no  = estCostMisc.itemID
                .
        END.        
        CREATE ttJobMatToKeep.
        ASSIGN 
            ttJobMatToKeep.riJobMat = ROWID(bf-job-mat)
            bf-job-mat.qty          = estCostMisc.quantityRequiredTotal
            bf-job-mat.qty-uom      = "EA"
            bf-job-mat.std-cost     = estCostMisc.costPerUOM
            bf-job-mat.sc-uom       = estCostMisc.costUOM
            bf-job-mat.cost-m       = estCostMisc.costTotalPerMFinished
            .
    END.    
    RELEASE bf-job-mat.
        
END PROCEDURE.

PROCEDURE pBuildMisc PRIVATE:
    /*------------------------------------------------------------------------------
        Purpose:  Given an estimateCostHeaderID and job buffer, build the job-prep
           records (or update them)
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-job    FOR job.
    
    DEFINE           BUFFER bf-job-prep FOR job-prep.
    
    FOR EACH estCostMisc NO-LOCK
        WHERE estCostMisc.estCostHeaderID EQ ipiEstCostHeaderID
        AND LOOKUP(estCostMisc.simon,"S,N,O") EQ 0
        AND (estCostMisc.itemID EQ "" 
            OR NOT CAN-FIND(FIRST ITEM WHERE ITEM.company EQ estCostMisc.company AND ITEM.i-no EQ estCostMisc.itemID)):
        FIND FIRST bf-job-prep EXCLUSIVE-LOCK 
            WHERE bf-job-prep.company EQ ipbf-job.company
            AND bf-job-prep.job EQ ipbf-job.job
            AND bf-job-prep.job-no EQ ipbf-job.job-no
            AND bf-job-prep.job-no2 EQ ipbf-job.job-no2
            AND bf-job-prep.frm EQ estCostMisc.formNo
            AND bf-job-prep.blank-no EQ estCostMisc.blankNo
            AND bf-job-prep.code EQ estCostMisc.itemID
            NO-ERROR.
        IF NOT AVAILABLE bf-job-prep THEN 
        DO:
            CREATE bf-job-prep.
            ASSIGN 
                bf-job-prep.company  = ipbf-job.company
                bf-job-prep.job      = ipbf-job.job
                bf-job-prep.job-no   = ipbf-job.job-no
                bf-job-prep.job-no2  = ipbf-job.job-no2
                bf-job-prep.frm      = estCostMisc.formNo
                bf-job-prep.blank-no = estCostMisc.blankNo
                bf-job-prep.code     = estCostMisc.prepID
                bf-job-prep.simon    = estCostMisc.simon
                .
        END.
        CREATE ttJobPrepToKeep.
        ASSIGN 
            ttJobPrepToKeep.riJobPrep = ROWID(bf-job-prep)
            bf-job-prep.cost-m = estCostMisc.costTotalPerMFinished
            bf-job-prep.qty = estCostMisc.quantityRequiredTotal
            bf-job-prep.ml = estCostMisc.costType EQ "Mat"
            bf-job-prep.std-cost = estCostMisc.costPerUOM
            bf-job-prep.sc-uom = estCostMisc.costUOM
            . 
            
    END.
    RELEASE bf-job-prep.

END PROCEDURE.

PROCEDURE pCalcEstimateForJob PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an estimate, and job, calculate an estimate creating job-specific
        estCost... records from which to build
     Notes:  Replaces jc/calc-est.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
    DEFINE INPUT PARAMETER ipdQuantityOverride AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstCostHeader AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-est     FOR est.
    
    DEFINE VARIABLE hdEstCalc         AS HANDLE  NO-UNDO.
    DEFINE VARIABLE dQuantityOverride AS DECIMAL NO-UNDO.
    
    IF ipdQuantityOverride EQ 0 THEN 
    DO:
        FIND FIRST bf-est NO-LOCK 
            WHERE bf-est.company EQ ipbf-job.company
            AND bf-est.est-no EQ ipbf-job.est-no
            NO-ERROR.
        IF AVAILABLE bf-est AND LOOKUP(STRING(bf-est.est-type),"1,2,5,6") GT 0 THEN 
        DO:  
            FIND FIRST bf-job-hdr NO-LOCK 
                WHERE bf-job-hdr.company EQ ipbf-job.company
                AND bf-job-hdr.job-no EQ ipbf-job.job-no
                AND bf-job-hdr.job-no2 EQ ipbf-job.job-no2
                NO-ERROR.
            IF AVAILABLE bf-job-hdr THEN 
                dQuantityOverride = bf-job-hdr.qty.
        END. 
    END.         
    ELSE 
        dQuantityOverride = ipdQuantityOverride.
    
    RUN est\EstimateCalcProcs.p PERSISTENT SET hdEstCalc.
    RUN CalculateJob IN hdEstCalc (ipbf-job.company, ipbf-job.est-no, ipbf-job.job-no, ipbf-job.job-no2, dQuantityOverride, YES, OUTPUT opiEstCostHeader).
    DELETE OBJECT hdEstCalc.

END PROCEDURE.

PROCEDURE pCleanJob PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Removes all uncalced or un-added job child records.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job    FOR job.
    
    DEFINE           BUFFER bf-job-mat  FOR job-mat.
    DEFINE           BUFFER bf-job-mch  FOR job-mch.
    DEFINE           BUFFER bf-job-prep FOR job-prep.
    DEFINE           BUFFER bf-job-farm FOR job-farm.
    DEFINE           BUFFER bf-job-hdr  FOR job-hdr.
    
    FOR EACH bf-job-mat EXCLUSIVE-LOCK
        WHERE bf-job-mat.company EQ ipbf-job.company
        AND bf-job-mat.job EQ ipbf-job.job
        AND bf-job-mat.job-no EQ ipbf-job.job-no
        AND bf-job-mat.job-no2 EQ ipbf-job.job-no2
        AND NOT CAN-FIND(FIRST ttJobMatToKeep WHERE ttJobMatToKeep.riJobMat EQ ROWID(bf-job-mat)):
        DELETE bf-job-mat.
    END.
    RELEASE bf-job-mat.
    
    FOR EACH bf-job-mch EXCLUSIVE-LOCK
        WHERE bf-job-mch.company EQ ipbf-job.company
        AND bf-job-mch.job EQ ipbf-job.job
        AND bf-job-mch.job-no EQ ipbf-job.job-no
        AND bf-job-mch.job-no2 EQ ipbf-job.job-no2
        AND NOT CAN-FIND(FIRST ttJobMchToKeep WHERE ttJobMchToKeep.riJobMch EQ ROWID(bf-job-mch)):
        DELETE bf-job-mch.
    END.
    RELEASE bf-job-mch.
    
    FOR EACH bf-job-prep EXCLUSIVE-LOCK
        WHERE bf-job-prep.company EQ ipbf-job.company
        AND bf-job-prep.job EQ ipbf-job.job
        AND bf-job-prep.job-no EQ ipbf-job.job-no
        AND bf-job-prep.job-no2 EQ ipbf-job.job-no2
        AND NOT CAN-FIND(FIRST ttJobPrepToKeep WHERE ttJobPrepToKeep.riJobPrep EQ ROWID(bf-job-prep)):
        DELETE bf-job-prep.
    END.
    RELEASE bf-job-prep.
    
    FOR EACH bf-job-farm EXCLUSIVE-LOCK
        WHERE bf-job-farm.company EQ ipbf-job.company
        AND bf-job-farm.job EQ ipbf-job.job
        AND bf-job-farm.job-no EQ ipbf-job.job-no
        AND bf-job-farm.job-no2 EQ ipbf-job.job-no2
        AND NOT CAN-FIND(FIRST ttJobFarmToKeep WHERE ttJobFarmToKeep.riJobFarm EQ ROWID(bf-job-farm)):
        DELETE bf-job-farm.
    END.
    RELEASE bf-job-farm.
    
    FOR EACH bf-job-hdr EXCLUSIVE-LOCK
        WHERE bf-job-hdr.company EQ ipbf-job.company
        AND bf-job-hdr.job EQ ipbf-job.job
        AND bf-job-hdr.job-no EQ ipbf-job.job-no
        AND bf-job-hdr.job-no2 EQ ipbf-job.job-no2
        AND NOT CAN-FIND(FIRST ttJobHdrToKeep WHERE ttJobHdrToKeep.riJobHdr EQ ROWID(bf-job-hdr)):
        DELETE bf-job-hdr.
    END.
    RELEASE bf-job-hdr.

END PROCEDURE.

