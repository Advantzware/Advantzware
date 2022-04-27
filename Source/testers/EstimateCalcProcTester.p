
/*------------------------------------------------------------------------
    File        : EstimateCalcProcTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Sakshi.Singh
    Created     : Thu Dec 12 03:16:52 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE VARIABLE ghSession           AS HANDLE.
DEFINE VARIABLE ghEstimateCalcProcs AS HANDLE.
DEFINE VARIABLE giTimer             AS INTEGER   NO-UNDO.

DEFINE VARIABLE gcProfilerFile      AS CHARACTER INITIAL "C:\temp\estCalcProfile.prof".
DEFINE VARIABLE gcCompany           AS CHARACTER INITIAL "001".
DEFINE VARIABLE gcEstimate          AS CHARACTER INITIAL "  101413".
DEFINE VARIABLE glDoJob             AS LOGICAL   INITIAL NO.
DEFINE VARIABLE glPurge             AS LOGICAL   INITIAL YES.
DEFINE VARIABLE glDoPrompts         AS LOGICAL   INITIAL YES.
DEFINE VARIABLE glCheckPerformace  AS LOGICAL   INITIAL No.

DEFINE VARIABLE cJobID              AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobID2             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEstCostHeaderID    AS INT64.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

/* ***************************  Main Block  *************************** */

RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).

RUN est\EstimateCalcProcs.p PERSISTENT SET ghEstimateCalcProcs.

RUN spSetSessionParam ("Company",  gcCompany).

// Turn On
IF glCheckPerformace THEN
    RUN pOnOffProfiler.
   
//RUN pBuildFreightForBoardCost (4800) . 

//RUN pTestEstimate.
// RUN pTestCalculateJobOrEst.    


RUN pCompareNewVsRefactoredProc.    

RUN pTestImportMachineStandards.

    
// Turn Off    
IF glCheckPerformace THEN
    RUN pOnOffProfiler.
    

PROCEDURE pTestEstimate:
    
    RUN CalculateEstimate IN ghEstimateCalcProcs(gcCompany,gcEstimate, glPurge).
END.    

PROCEDURE pCompareNewVsRefactoredProc:
    
    DEFINE VARIABLE cCompany  AS CHARACTER NO-UNDO INITIAL "001".
    DEFINE VARIABLE cEstID1   AS CHARACTER NO-UNDO INITIAL "  103423".
    DEFINE VARIABLE cEstID2   AS CHARACTER NO-UNDO INITIAL "  103423".
    DEFINE VARIABLE iSrcHeaderID AS INT64 NO-UNDO.
    DEFINE VARIABLE iTrgHeaderID AS INT64 NO-UNDO.
    DEFINE VARIABLE cFields  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEstList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCnt     AS INTEGER NO-UNDO.
    DEFINE VARIABLE cTime    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
    
    
    DEFINE BUFFER bf-SrcEstCostDetail FOR EstCostDetail.
    DEFINE BUFFER bf-TrgEstCostDetail FOR EstCostDetail.
    DEFINE BUFFER bf-SrcEstCostForm FOR EstCostForm.
    DEFINE BUFFER bf-TrgEstCostForm FOR EstCostForm.
    //cEstList = "103412,103413,103416,103417,103423,103425".
     cEstList = "103363,103368,103370,103404,103418".
    //cEstList = "103436".
    
    cEstList = "103425".
    /*
    Mismatch 103418 
    Source Id and Target ID same- 103413 , 103404 , 103363
    */
    
    DO iCnt = 1 to NUM-ENTRIES(cEstList):
    
        ASSIGN
            cEstID1 = ENTRY(icnt,cEstList)
            cEstID1 = FILL(" ", 8 - LENGTH(cEstID1)) + cEstID1
            iSrcHeaderID = 0
            iTrgHeaderID = 0
            cTime      = "".
            
            
        eTime(Yes).
        RUN CalculateEstimate IN ghEstimateCalcProcs(cCompany,cEstID1, YES).
        
        cTime = STRING(eTime).
                   
        FOR LAST PROBE NO-LOCK
            WHERE probe.company = cCompany  
            AND probe.est-no = cEstID1,    
            FIRST estCostHeader NO-LOCK 
            WHERE estCostHeader.estCostHeaderID EQ INT64(probe.spare-char-2):
            iSrcHeaderID = estCostHeader.estCostHeaderID.
        END. 
      
        /*
        IF NOT CAN-FIND(FIRST bf-SrcEstCostForm WHERE bf-SrcEstCostForm.estCostHeaderID = iSrcHeaderID) THEN
        DO:
            MESSAGE
            "Est" cEstID1 skip
             "NOT AVAILABLE bf-bf-SrcEstCostForm: " iSrcHeaderID
                VIEW-AS ALERT-BOX.
            NEXT.
        END.
        
        cText = "". 
            
        FOR EACH bf-SrcEstCostForm NO-LOCK
            WHERE bf-SrcEstCostForm.estCostHeaderID = iSrcHeaderID:
        
                    
            FIND FIRST bf-TrgEstCostForm NO-LOCK
                WHERE bf-TrgEstCostForm.estCostHeaderID = iTrgHeaderID
                AND bf-TrgEstCostForm.formNo = bf-SrcEstCostForm.formNo
                NO-ERROR.
        
            IF NOT AVAILABLE bf-TrgEstCostForm THEN
            DO:
                MESSAGE "NOT AVAILABLE bf-TrgEstCostForm: " iTrgHeaderID
                    VIEW-AS ALERT-BOX.
                NEXT.
            END.
      
            BUFFER-COMPARE bf-SrcEstCostForm to bf-TrgEstCostForm save result in cFields.
    
    
            IF cFields <> "" THEN
            DO:                
                cText = cText +  "Est" + cEstID1 + Chr(10)
                       + "Form" + String(bf-SrcEstCostForm.formNo) + Chr(10)
                       + "Fields:" + cFields + Chr(10).
            END.
               
        END. // bf-SrcEstCostForm
    */
    
        IF NOT CAN-FIND(FIRST bf-SrcEstCostDetail WHERE bf-SrcEstCostDetail.estCostHeaderID = iSrcHeaderID) THEN
        DO:
            MESSAGE
                "Est" cEstID1 skip
                "NOT AVAILABLE bf-bf-SrcEstCostDetail: " iSrcHeaderID
                VIEW-AS ALERT-BOX.
            NEXT.
        END.
        
        cText = "". 
            
        FOR EACH bf-SrcEstCostDetail NO-LOCK
            WHERE bf-SrcEstCostDetail.estCostHeaderID = iSrcHeaderID:
        
                    
            FIND FIRST bf-TrgEstCostDetail NO-LOCK
                WHERE bf-TrgEstCostDetail.estCostHeaderID = iTrgHeaderID
                AND bf-TrgEstCostDetail.sourceType = bf-SrcEstCostDetail.sourceType
                AND bf-TrgEstCostDetail.estCostCategoryID = bf-SrcEstCostDetail.estCostCategoryID
                
                NO-ERROR.
        
            IF NOT AVAILABLE bf-TrgEstCostDetail THEN
            DO:
                MESSAGE "NOT AVAILABLE bf-TrgEstCostDetail: " iTrgHeaderID
                    VIEW-AS ALERT-BOX.
                NEXT.
            END.
      
            BUFFER-COMPARE bf-SrcEstCostDetail to bf-TrgEstCostDetail save result in cFields.
    
    
            IF cFields <> "" THEN
            DO:                
                cText = cText +  "Est" + cEstID1 + Chr(10)
                    + "CategoryID" + String(bf-SrcEstCostDetail.estCostCategoryID) + Chr(10)
                    + "Fields:" + cFields + Chr(10).
            END.
               
        END. // bf-SrcEstCostDetail
    
    
        IF cText = "" THEN
            MESSAGE 
                "Est" cEstID1 skip
                "cTime" cTime skip
                "Match"
                VIEW-AS ALERT-BOX. 
        ELSE
            MESSAGE 
                "Est" cEstID1 skip
                "cTime" cTime skip
                "cText" cText
                VIEW-AS ALERT-BOX.
    
    END. 
END.

PROCEDURE pTestCalculateJobOrEst:
    
    IF glDoJob THEN
    DO: 
        FIND FIRST job NO-LOCK 
            WHERE job.company EQ gcCompany
            AND job.est-no EQ gcEstimate
            NO-ERROR.

        IF AVAILABLE job AND glDoJob THEN 
            ASSIGN 
                cJobID  = job.job-no
                iJobID2 = job.job-no2
                .
            
        IF glDoPrompts THEN
            RUN CalculateJobWithPrompts(gcCompany,gcEstimate, cJobID, iJobID2, 0, glPurge, OUTPUT iEstCostHeaderID).
        ELSE  
            RUN CalculateJob(gcCompany,gcEstimate, cJobID, iJobID2, 0, glPurge, OUTPUT iEstCostHeaderID).
    END.
    ELSE 
        IF glDoPrompts THEN
            RUN CalculateEstimateWithPrompts(gcCompany,gcEstimate, glPurge).
        ELSE 
            RUN CalculateEstimate(gcCompany,gcEstimate, glPurge).
END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pOnOffProfiler :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE lProfile          AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iProfileStartTime AS INTEGER NO-UNDO.
    
    IF PROFILER:ENABLED THEN 
    DO:
        ASSIGN 
            PROFILER:PROFILING = FALSE                         
            PROFILER:ENABLED   = FALSE
            iProfileStartTime  = TIME                 
            . 
        PROFILER:WRITE-DATA().
    END.
    ELSE 
    DO:
        ASSIGN  
            PROFILER:ENABLED      = TRUE
            PROFILER:DESCRIPTION  = STRING(TODAY,"999999") + "_" + STRING(TIME, "HH:MM:SS")
            PROFILER:FILE-NAME    = gcProfilerFile
            PROFILER:PROFILING    = TRUE
            PROFILER:TRACE-FILTER = "*"
            iProfileStartTime     = TIME 
            .
    END. 
   

END PROCEDURE.


PROCEDURE pBuildFreightForBoardCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Determine the Freight charges for Board Material
     Notes: This cost is processing for Single/combo estimates but not being included in case of Item-BOM setup.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ef                   FOR ef. 
    DEFINE BUFFER bf-estCostForm          FOR estCostForm. 
    DEFINE BUFFER bf-estCostBlank         FOR estCostBlank. 
    DEFINE BUFFER bf-estCostHeader        FOR estCostHeader.
    DEFINE BUFFER bf-estCostItem          FOR estCostItem. 
    
    
    DEFINE VARIABLE dBoardFreight   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityInCUOM   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityInSrcUOM AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE cSrcUOM         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPerMSheetUOM   AS CHARACTER NO-UNDO INIT "MSH".
    DEFINE VARIABLE cTargetUOM AS CHARACTER NO-UNDO.
    
    
   
    FOR FIRST bf-estCostHeader NO-LOCK 
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH bf-estCostForm NO-LOCK
        WHERE bf-estCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST bf-ef NO-LOCK
        WHERE bf-ef.company = bf-estCostForm.company
        AND bf-ef.est-no  = bf-estCostForm.estimateNo
        AND bf-ef.form-no = bf-estCostForm.formNo 
        AND bf-ef.fr-msh  NE 0:
            
        ASSIGN
            dQuantityInCUOM = 0
            dBoardFreight  = 0.
        
        /*IF bf-ef.fr-uom NE "" THEN
        DO: 
        
            IF bf-ef.fr-uom = bf-estCostForm.grossQtyRequiredTotalAreaUOM THEN
                dQuantityInCUOM = bf-estCostForm.grossQtyRequiredTotalArea.
        
            ELSE
            DO: 
                ASSIGN
                    dQuantityInSrcUOM = bf-estCostForm.grossQtyRequiredTotalWeight 
                    cSrcUOM           = bf-estCostForm.grossQtyRequiredTotalWeightUOM. 
    
                IF bf-ef.fr-uom = cPerMSheetUOM THEN
                    ASSIGN
                        dQuantityInSrcUOM = bf-estCostForm.grossQtyRequiredTotal 
                        cSrcUOM           = "EA".
                
                RUN pConvertQuantityFromUOMToUOM(bf-estCostForm.company, bf-ef.board, "RM", cSrcUOM, bf-ef.fr-uom, 
                    bf-estCostForm.basisWeight, bf-estCostForm.grossLength, bf-estCostForm.grossWidth, bf-estCostForm.grossDepth, 
                    dQuantityInSrcUOM, OUTPUT dQuantityInCUOM).
            END.
        END. 
        */
        
        
        
        
        ASSIGN
            dQuantityInSrcUOM = bf-estCostForm.grossQtyRequiredTotalArea
            cSrcUOM           = bf-estCostForm.grossQtyRequiredTotalAreaUOM
            cTargetUOM = "MSH"
            . 
            
            IF cTargetUOM = cPerMSheetUOM THEN
                    ASSIGN
                        dQuantityInSrcUOM = bf-estCostForm.grossQtyRequiredTotal 
                        cSrcUOM           = "EA".

         RUN pConvertQuantityFromUOMToUOM(bf-estCostForm.company, bf-ef.board, "RM", cSrcUOM, cTargetUOM, 
                    bf-estCostForm.basisWeight, bf-estCostForm.grossLength, bf-estCostForm.grossWidth, bf-estCostForm.grossDepth, 
                    dQuantityInSrcUOM, OUTPUT dQuantityInCUOM).
        
        IF dQuantityInCUOM NE 0 THEN
            dBoardFreight = dQuantityInCUOM *  bf-ef.fr-msh. 
            
            
            
            MESSAGE 
            "cSrcUOM" cSrcUOM skip
            "dQuantityInSrcUOM" dQuantityInSrcUOM skip
            "cTargetUOM" cTargetUOM skip
            "dQuantityInCUOM" dQuantityInCUOM skip
            "dBoardFreight" dBoardFreight
            
            VIEW-AS ALERT-BOX.
                    
    
    END.
    
END PROCEDURE.

PROCEDURE pTestImportMachineStandards:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-est-op           FOR est-op.
    DEFINE BUFFER bf-estCostOperation FOR estCostOperation.
    
    
    DEFINE VARIABLE dMRWaste   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMRHrs     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSpeed     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSpoilPrct AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ghOperation AS HANDLE NO-UNDO.
    
    RUN est\OperationProcs.p PERSISTENT SET ghOperation.
    
    FOR EACH bf-est-op NO-LOCk
        WHERE bf-est-op.Company = "001"
        AND bf-est-op.est-no  = "  104045"
        //AND bf-est-op.qty  = 1000
        
        and bf-est-op.m-code = "408"
            by bf-est-op.line desc:
              
        FOR FIRST bf-estCostOperation NO-LOCK
            WHERE bf-estCostOperation.company    = bf-est-op.company
            AND bf-estCostOperation.estimateNo   = bf-est-op.est-No
            AND bf-estCostOperation.formNo       = bf-est-op.s-num
            AND bf-estCostOperation.blankNo      = bf-est-op.b-num
            AND bf-estCostOperation.operationID  = bf-est-op.m-code:
       
            RUN Operations_ImportMachineStandards IN ghOperation
                (bf-est-op.company, bf-est-op.est-no, bf-est-op.s-num, bf-est-op.b-num, bf-est-op.op-pass,bf-est-op.qty, 600000, bf-est-op.m-code, OUTPUT dSpeed, OUTPUT dMRHrs, OUTPUT dMRWaste, OUTPUT dSpoilPrct).
    
    
            MESSAGE
            bf-est-op.m-code skip
           
                bf-estCostOperation.quantityInSetupWaste  dMRWaste skip 
                bf-estCostOperation.hoursSetup           dMRHrs skip
                bf-estCostOperation.speed   dSpeed skip
                bf-estCostOperation.quantityInRunWastePercent dSpoilPrct skip
                VIEW-AS ALERT-BOX.
                
        END.
    END.
   
END PROCEDURE.
