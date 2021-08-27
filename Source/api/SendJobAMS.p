
/*------------------------------------------------------------------------
    File        : api/SendJobAMS.p
    Purpose     : Returns the request data for a Job (job-hdr, job-mch, 
                  job-mat, job-prep)
    Syntax      :

    Description : Returns the request data for a job(job-hdr, job-mch, 
                  job-mat, job-prep)

    Author(s)   : DEVA$!
    Created     : Mon Jun 14 06:25:42 EST 2021
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

DEFINE TEMP-TABLE ttLink NO-UNDO
    FIELD action       AS CHARACTER
    FIELD toPart       AS CHARACTER
    FIELD fromPart     AS CHARACTER
    FIELD toTask       AS CHARACTER
    FIELD fromTask     AS CHARACTER
    FIELD toLocation   AS CHARACTER
    FIELD fromLocation AS CHARACTER
    FIELD formNo       AS INTEGER 
    FIELD blankNo      AS INTEGER
    FIELD passNo       AS INTEGER
    FIELD seqNo        AS INTEGER
    FIELD isCombo      AS LOGICAL
    FIELD isAssembly   AS LOGICAL
    FIELD requestData  AS CLOB
    INDEX idx isAssembly formNo blankNo
    .         
    
DEFINE TEMP-TABLE ttPart NO-UNDO
    FIELD formNo       AS INTEGER 
    FIELD blankNo      AS INTEGER
    FIELD partID       AS CHARACTER
    FIELD location     AS CHARACTER
    FIELD taskIDs      AS CHARACTER
    FIELD comboHdr     AS LOGICAL
    FIELD comboPart    AS LOGICAL
    FIELD setAssembly  AS LOGICAL
    FIELD dieInches    AS DECIMAL
    FIELD requestData  AS CLOB
    . 

DEFINE TEMP-TABLE ttTask NO-UNDO
    FIELD formNo       AS INTEGER 
    FIELD blankNo      AS INTEGER
    FIELD passNo       AS INTEGER
    FIELD partID       AS CHARACTER
    FIELD location     AS CHARACTER
    FIELD taskID       AS INTEGER
    FIELD lineID       AS INTEGER
    FIELD runQuantity  AS DECIMAL
    FIELD requestData  AS CLOB
    . 

DEFINE TEMP-TABLE ttMaterial NO-UNDO
    FIELD formNo       AS INTEGER 
    FIELD blankNo      AS INTEGER
    FIELD passNo       AS INTEGER
    FIELD partID       AS CHARACTER
    FIELD lineID       AS INTEGER
    FIELD requestData  AS CLOB
    .
    
/* Variables to job's request data */
DEFINE VARIABLE lcJobsData                    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobsData              AS LONGCHAR  NO-UNDO.

/* Variables to store job material request data */
DEFINE VARIABLE lcJobHeaderData               AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcTempJobHeaderData           AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobHeaderData         AS LONGCHAR  NO-UNDO.

/* Variables to store job material request data */
DEFINE VARIABLE lcJobMatData                  AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobMatData            AS LONGCHAR  NO-UNDO.
    
/* Variables to store job machine request data */
DEFINE VARIABLE lcJobMachineData              AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobMachineData        AS LONGCHAR  NO-UNDO.

/* Variables to store job header set parts data */
DEFINE VARIABLE lcJobSetPartData              AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobSetPartData        AS LONGCHAR  NO-UNDO.
    
/* Variables to store job machine request data */
DEFINE VARIABLE lcJobLinkData                 AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobLinkData           AS LONGCHAR  NO-UNDO.

/* Variables to store job Preperation request data */
DEFINE VARIABLE lcJobPrepData                 AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobPrepData           AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cCompany                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo2                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstimate                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCSRID                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCSRName                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobStatus                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEnteredBy                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobDueDate                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobDueTime                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobStartDate                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNotes                        AS CHARACTER NO-UNDO.
    
/* Job Header Variables*/
DEFINE VARIABLE iTaskCounter                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLinkCounter                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lIsSet                        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lIsSetHeader                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iHeaderLink                   AS INTEGER   NO-UNDO.                
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
DEFINE VARIABLE dOrderQty                     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cMfgDate                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBoardCode                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cGrain                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCylinder                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTray                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPriority                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIsPriority                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKeyItem                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNumberOn                     AS CHARACTER NO-UNDO.
    
/*Job Material variables*/
    
DEFINE VARIABLE cItemNumber                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMatItemStandardCost       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStandardCostUOM              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCostPerUOM                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQtyToOrder                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQuantityUOM                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemCost                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLength                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWidth                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBasisWeight                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLineNumber                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRMItem                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMatBlank                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMatForm                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAllocated                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQuantityAllocated            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQuantityIssued               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRQuantity                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWasteQuantity                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDepth                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPONumber                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCrossGrain                   AS CHARACTER NO-UNDO.
    
/*Job Machine variables*/
    
DEFINE VARIABLE cAnchored                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMachineCode                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRWaste                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRRate                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMchMRvariableOverheadRate    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMchMRFixedOverheadRate       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRFixedOverheadCost          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDepartment                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemName                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMchForm                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMchBlank                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMchItem                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInteger                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunHours                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunMinutes                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunSpeed                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRHours                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRMinutes                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMchLineNumber             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunQuantity                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunStartDate                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunStartTime                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQueueTime                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLagTime                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndDate                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndTime                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSetupStartDate               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSetupStartTime               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSetupEndDate                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSetupEndTime                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRComplete                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunComplete                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMchDueDate                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMchDueTime                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRTotalRate                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFixedOverheadRate            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunProfitPct                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRProfitPct                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunRate                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunTotalRate                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVariableOverheadRate         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRContributionRate           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunContributionRate          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMachineID                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobMchWastePct               AS CHARACTER NO-UNDO.
    
/* Job Preps Variables */
    
DEFINE VARIABLE cPrepCode                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobPrepQuantity              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemStandardCost             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStandardUOM                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCostPerM                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrepDate                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrepTime                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobPrepPosted                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobPrepForm                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobPrepBlank                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSIMON                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMatOrLab                     AS CHARACTER NO-UNDO.

/* Variables associated with job header parts  */
DEFINE VARIABLE cPartQuantity                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPartID                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPartName                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDieInches                    AS CHARACTER NO-UNDO.

DEFINE VARIABLE lIsNewCalculationMethod       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iFormCounter                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBlankCounter                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE lIsACombo                     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iLinkSeqNo                    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iAssemblyBlankCount           AS INTEGER   NO-UNDO.
                    
DEFINE BUFFER bf-APIOutboundDetail        FOR APIOutboundDetail.
DEFINE BUFFER bf-job-mat                  FOR job-mat.
DEFINE BUFFER bf-users                    FOR users.
DEFINE BUFFER bf-notes                    FOR notes.

/**********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

RUN pSetRequestDataType (INPUT "XML").

IF ipcRequestHandler NE "" THEN
    RUN VALUE(ipcRequestHandler) (
        INPUT TABLE ttArgs,
        INPUT ipiAPIOutboundID,
        INPUT ipiAPIOutboundTriggerID,
        INPUT-OUTPUT ioplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ). 
ELSE 
DO:
    FIND FIRST APIOutboundDetail NO-LOCK
         WHERE APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND APIOutboundDetail.detailID      EQ "JobDetails"
           AND APIOutboundDetail.parentID      EQ "SendJobAMS"
         NO-ERROR.
        
    IF NOT AVAILABLE APIOutboundDetail THEN 
    DO:
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
    IF NOT AVAILABLE ttArgs THEN 
    DO:
        ASSIGN
            opcMessage = "No valid job record passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
        
    FIND FIRST job NO-LOCK
         WHERE ROWID(job) EQ TO-ROWID(ttArgs.argValue)
         NO-ERROR.
    IF NOT AVAILABLE job THEN 
    DO:
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
        cJobStatus    = IF job.stat EQ "C" OR job.stat EQ "Z" THEN
                            "Closed"
                        ELSE IF job.stat EQ "H" THEN
                            "OnHold"
                        ELSE
                            "ProductionReady"
        cEnteredBy    = job.user-id
        cJobDueDate   = STRING(job.due-date)
        cJobDueTime   = STRING(job.due-time)
        cJobStartDate = STRING(job.start-date)
        .
    
    FIND FIRST bf-users NO-LOCK
         WHERE bf-users.user_id EQ cCSRID
         NO-ERROR.
    IF AVAILABLE bf-users THEN
        cCSRName = bf-users.user_name.    

    FOR EACH bf-notes NO-LOCK
        WHERE bf-notes.rec_key   EQ job.rec_key
          AND bf-notes.note_code EQ "SC":
        cNotes = bf-notes.note_text + " " + cNotes. 
    END.
                            
    RUN pProcessAMSData(BUFFER job).
    
    FOR EACH ttPart:
        lcJobHeaderData = ttPart.requestData.
        
        lcConcatJobHeaderData = lcConcatJobHeaderData + lcJobHeaderData. 
    END.
                
    FOR EACH ttMaterial:        
        lcJobMatData = ttMaterial.requestData.
                        
        lcConcatJobMatData  = lcConcatJobMatData + lcJobMatData.                                              
    END.
    
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ "JobMachine"
           AND bf-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN 
    DO:  
        FOR EACH job-mch NO-LOCK
            WHERE job-mch.company   EQ job.company
              AND job-mch.job       EQ job.job
              AND job-mch.job-no    EQ job.job-no
              AND job-mch.job-no2   EQ job.job-no2
            USE-INDEX line-idx:

            lcJobMachineData = bf-APIOutboundDetail.data.

            RUN pUpdateMachineDetails(
                BUFFER job-mch,
                INPUT  lcJobMachineData,
                OUTPUT lcJobMachineData
                ).
        
            lcConcatJobMachineData = lcConcatJobMachineData + lcJobMachineData.
        END.
    END.
    
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ "JobPreps"
           AND bf-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
         NO-ERROR. 
    IF AVAILABLE bf-APIOutboundDetail THEN 
    DO:  
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

    FOR EACH ttLink
        WHERE ttLink.toTask NE "":
        lcJobLinkData = ttLink.requestData.
        
        lcConcatJobLinkData = lcConcatJobLinkData + lcJobLinkData.
    END.
            
    lcJobsData       = REPLACE(lcJobsData, "$JobHeader$", lcConcatJobHeaderData).
    lcJobsData       = REPLACE(lcJobsData, "$JobMaterial$", lcConcatJobMatData).
    lcJobsData       = REPLACE(lcJobsData, "$JobMachine$", lcConcatJobMachineData).
    lcJobsData       = REPLACE(lcJobsData, "$JobPrep$", lcConcatJobPrepData).
    lcJobsData       = REPLACE(lcJobsData, "$JobSetPart$", lcConcatJobSetPartData).
    lcJobsData       = REPLACE(lcJobsData, "$JobLink$", lcConcatJobLinkData).
        
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Company",cCompany).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobNumber1",cJobNo).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobNumber2",cJobNo2).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Priority",cPriority).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "IsPriority",cIsPriority).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Location",cLocation).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Estimate",cEstimate).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "CSRID",cCSRID).              
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "CSRName",cCSRName).  
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "EnteredBy",cEnteredBy).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobDueDate",cJobDueDate).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobDueTime",cJobDueTime).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobStartDate",cJobStartDate).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "Notes",cNotes).
    RUN updateRequestData(INPUT-OUTPUT lcJobsData, "JobStatus",cJobStatus).

    ioplcRequestData = REPLACE(ioplcRequestData, "$Jobs$", lcJobsData).   
END.                        
    
ASSIGN
    oplSuccess = TRUE
    opcMessage = "Success"
    .

PROCEDURE pCreateLinks PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplIsSet AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-ttLink                   FOR ttLink.
    DEFINE BUFFER bf-combo-ttLink             FOR ttLink.                    
    
    DEFINE VARIABLE iAssemblySeqNo AS INTEGER  NO-UNDO.
    DEFINE VARIABLE lFound         AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lcJobLinkData  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE iTaskCounter   AS INTEGER  NO-UNDO.
    
    DEFINE BUFFER bf-link-APIOutboundDetail   FOR APIOutboundDetail.
    DEFINE BUFFER bf-ttPart                   FOR ttPart.
    DEFINE BUFFER bf-to-ttTask                FOR ttTask.
    DEFINE BUFFER bf-from-ttTask              FOR ttTask.
    
    /* Create ttLinks for combo forms where 0 blank should have links to all other blanks */
    FOR EACH ttPart
        WHERE ttPart.comboHdr
          AND NOT ttPart.setAssembly:
        DO iTaskCounter = 1 TO NUM-ENTRIES(ttPart.taskIDs):
            FIND FIRST bf-from-ttTask 
                 WHERE bf-from-ttTask.taskID EQ INTEGER(ENTRY(iTaskCounter, ttPart.taskIDs)) 
                 NO-ERROR.
            IF NOT AVAILABLE bf-from-ttTask THEN
                NEXT.
            
            /* Find all the combo parts for the combo header and create links */
            FOR EACH bf-ttPart
                WHERE bf-ttPart.formNo EQ ttPart.formNo
                  AND NOT bf-ttPart.comboHdr
                  AND bf-ttPart.comboPart:
                /* Find the first task of the combo part and create a link */
                FIND FIRST bf-to-ttTask
                     WHERE bf-to-ttTask.taskID EQ INTEGER(ENTRY(1, bf-ttPart.taskIDs))
                     NO-ERROR.
                IF AVAILABLE bf-to-ttTask THEN DO:
                    FIND FIRST ttLink 
                         WHERE ttLink.fromTask EQ STRING(bf-from-ttTask.taskID)
                           AND ttLink.toTask   EQ STRING(bf-to-ttTask.taskID)
                         NO-ERROR.
                    IF NOT AVAILABLE ttLink THEN
                        CREATE ttLink.                

                    ASSIGN
                        iLinkSeqNo          = iLinkSeqNo + 1
                        ttLink.fromPart     = bf-from-ttTask.partID
                        ttLink.fromTask     = STRING(bf-from-ttTask.taskID)
                        ttLink.fromLocation = bf-from-ttTask.location
                        ttLink.toPart       = bf-to-ttTask.partID
                        ttLink.toTask       = STRING(bf-to-ttTask.taskID)
                        ttLink.toLocation   = bf-to-ttTask.location
                        ttLink.formNo       = bf-from-ttTask.formNo
                        ttLink.blankNo      = bf-from-ttTask.blankNo
                        ttLink.passNo       = bf-from-ttTask.passNo
                        ttLink.isCombo      = TRUE
                        ttLink.seqNo        = (ttLink.formNo * 1000) + ttLink.blankNo + ttLink.passNo + iLinkSeqNo
                        .
                END.
            END.    
        END. 
    END.

    /* Create a link between tasks of same form, blank but different pass */
/*    FOR EACH bf-from-ttTask:                                                                              */
/*        FIND FIRST bf-to-ttTask                                                                           */
/*             WHERE bf-to-ttTask.formNo  EQ bf-from-ttTask.formNo                                          */
/*               AND bf-to-ttTask.blankNo EQ bf-from-ttTask.blankNo                                         */
/*               AND bf-to-ttTask.passNo  GT bf-from-ttTask.passNo                                          */
/*               AND bf-to-ttTask.lineID  GT bf-from-ttTask.lineID                                          */
/*             NO-ERROR.                                                                                    */
/*        IF AVAILABLE bf-to-ttTask THEN DO:                                                                */
/*            FIND FIRST ttLink                                                                             */
/*                 WHERE ttLink.fromTask EQ STRING(bf-from-ttTask.taskID)                                   */
/*                   AND ttLink.toTask   EQ STRING(bf-to-ttTask.taskID)                                     */
/*                 NO-ERROR.                                                                                */
/*            IF NOT AVAILABLE ttLink THEN                                                                  */
/*                CREATE ttLink.                                                                            */
/*                                                                                                          */
/*            ASSIGN                                                                                        */
/*                iLinkSeqNo          = iLinkSeqNo + 1                                                      */
/*                ttLink.toPart       = bf-to-ttTask.partID                                                 */
/*                ttLink.toTask       = STRING(bf-to-ttTask.taskID)                                         */
/*                ttLink.toLocation   = bf-to-ttTask.location                                               */
/*                ttLink.fromPart     = bf-from-ttTask.partID                                               */
/*                ttLink.fromTask     = STRING(bf-from-ttTask.taskID)                                       */
/*                ttLink.fromLocation = bf-from-ttTask.location                                             */
/*                ttLink.formNo       = bf-from-ttTask.formNo                                               */
/*                ttLink.blankNo      = bf-from-ttTask.blankNo                                              */
/*                ttLink.passNo       = bf-from-ttTask.passNo                                               */
/*                ttLink.seqNo        = (ttLink.formNo * 1000) + ttLink.blankNo + ttLink.passNo + iLinkSeqNo*/
/*                .                                                                                         */
/*        END.                                                                                              */
/*    END.                                                                                                  */

    /* Create ttLinks for non-combo forms */
    FOR EACH ttPart
        WHERE NOT ttPart.comboHdr
          AND NOT ttPart.comboPart
          AND NOT ttPart.setAssembly:
        DO iTaskCounter = 1 TO NUM-ENTRIES(ttPart.taskIDs):
            FIND FIRST bf-from-ttTask 
                 WHERE bf-from-ttTask.taskID EQ INTEGER(ENTRY(iTaskCounter, ttPart.taskIDs)) 
                 NO-ERROR.
            IF NOT AVAILABLE bf-from-ttTask THEN
                NEXT.
            
            /* Find the next task of the part and create a link */
            FIND FIRST bf-to-ttTask
                 WHERE bf-to-ttTask.formNo  EQ bf-from-ttTask.formNo
/*                   AND bf-to-ttTask.blankNo GT bf-from-ttTask.blankNo*/
                   AND bf-to-ttTask.lineID  GT bf-from-ttTask.lineID
                 NO-ERROR.
            IF AVAILABLE bf-to-ttTask THEN DO:
                FIND FIRST ttLink 
                     WHERE ttLink.fromTask EQ STRING(bf-from-ttTask.taskID)
                       AND ttLink.toTask   EQ STRING(bf-to-ttTask.taskID)
                     NO-ERROR.
                IF NOT AVAILABLE ttLink THEN
                    CREATE ttLink.

                ASSIGN
                    iLinkSeqNo          = iLinkSeqNo + 1
                    ttLink.fromPart     = bf-from-ttTask.partID
                    ttLink.fromTask     = STRING(bf-from-ttTask.taskID)
                    ttLink.fromLocation = bf-from-ttTask.location
                    ttLink.toPart       = bf-to-ttTask.partID
                    ttLink.toTask       = STRING(bf-to-ttTask.taskID)
                    ttLink.toLocation   = bf-to-ttTask.location
                    ttLink.formNo       = bf-from-ttTask.formNo
                    ttLink.blankNo      = bf-from-ttTask.blankNo
                    ttLink.passNo       = bf-from-ttTask.passNo
                    ttLink.seqNo        = (ttLink.formNo * 1000) + ttLink.blankNo + ttLink.passNo + iLinkSeqNo
                    .
            END.
        END. 
    END.          
    
    IF iplIsSet THEN DO:
        /* Link combo parts to set assembly part */
        FOR EACH ttPart
            WHERE NOT ttPart.comboHdr
              AND ttPart.comboPart
              AND NOT ttPart.setAssembly:
            FIND FIRST bf-from-ttTask 
                 WHERE bf-from-ttTask.taskID EQ INTEGER(ENTRY(NUM-ENTRIES(ttPart.taskIDs), ttPart.taskIDs)) 
                 NO-ERROR.
            IF NOT AVAILABLE bf-from-ttTask THEN
                NEXT.
              
            FIND FIRST bf-ttPart
                 WHERE bf-ttPart.setAssembly
                 NO-ERROR.
            IF AVAILABLE bf-ttPart THEN DO:                
                /* Find the first task of the combo part and create a link */
                FIND FIRST bf-to-ttTask
                     WHERE bf-to-ttTask.taskID EQ INTEGER(ENTRY(1, bf-ttPart.taskIDs)) 
                     NO-ERROR.
                IF AVAILABLE bf-to-ttTask THEN DO:
                    FIND FIRST ttLink 
                         WHERE ttLink.fromTask EQ STRING(bf-from-ttTask.taskID)
                           AND ttLink.toTask   EQ STRING(bf-to-ttTask.taskID)
                         NO-ERROR.
                    IF NOT AVAILABLE ttLink THEN
                        CREATE ttLink.

                    ASSIGN
                        iLinkSeqNo          = iLinkSeqNo + 1
                        ttLink.fromPart     = bf-from-ttTask.partID
                        ttLink.fromTask     = STRING(bf-from-ttTask.taskID)
                        ttLink.fromLocation = bf-from-ttTask.location
                        ttLink.toPart       = bf-to-ttTask.partID
                        ttLink.toTask       = STRING(bf-to-ttTask.taskID)
                        ttLink.toLocation   = bf-to-ttTask.location
                        ttLink.formNo       = bf-from-ttTask.formNo
                        ttLink.blankNo      = bf-from-ttTask.blankNo
                        ttLink.passNo       = bf-from-ttTask.passNo
                        ttLink.isAssembly   = TRUE
                        ttLink.seqNo        = (ttLink.formNo * 1000) + ttLink.blankNo + ttLink.passNo + iLinkSeqNo
                        .
                END.
            END. 
        END. 

        /* Link last task of non combo form to the set assembly task */
        FOR EACH ttPart
            WHERE NOT ttPart.comboHdr
              AND NOT ttPart.comboPart
              AND NOT ttPart.setAssembly:
            FIND FIRST bf-from-ttTask 
                 WHERE bf-from-ttTask.taskID EQ INTEGER(ENTRY(NUM-ENTRIES(ttPart.taskIDs), ttPart.taskIDs)) 
                 NO-ERROR.
            IF NOT AVAILABLE bf-from-ttTask THEN
                NEXT.
            
            FIND FIRST bf-ttPart
                 WHERE bf-ttPart.setAssembly
                 NO-ERROR.
            IF AVAILABLE bf-ttPart THEN DO:
                FIND FIRST bf-to-ttTask
                     WHERE bf-to-ttTask.taskID EQ INTEGER(ENTRY(1, bf-ttPart.taskIDs)) 
                     NO-ERROR.
                IF AVAILABLE bf-to-ttTask THEN DO:            
                    FIND FIRST ttLink 
                         WHERE ttLink.fromTask EQ STRING(bf-from-ttTask.taskID)
                           AND ttLink.toTask   EQ STRING(bf-to-ttTask.taskID)
                         NO-ERROR.
                    IF NOT AVAILABLE ttLink THEN
                        CREATE ttLink.
                    
                    ASSIGN
                        iLinkSeqNo          = iLinkSeqNo + 1
                        ttLink.fromPart     = bf-from-ttTask.partID
                        ttLink.fromTask     = STRING(bf-from-ttTask.taskID)
                        ttLink.fromLocation = bf-from-ttTask.location
                        ttLink.toPart       = bf-to-ttTask.partID
                        ttLink.toTask       = STRING(bf-to-ttTask.taskID)
                        ttLink.toLocation   = bf-to-ttTask.location
                        ttLink.formNo       = bf-from-ttTask.formNo
                        ttLink.blankNo      = bf-from-ttTask.blankNo
                        ttLink.passNo       = bf-from-ttTask.passNo
                        ttLink.isAssembly   = TRUE
                        ttLink.seqNo        = (ttLink.formNo * 1000) + ttLink.blankNo + ttLink.passNo + iLinkSeqNo
                        .
                END.
            END.
        END.          
    END.

    FIND FIRST bf-link-APIOutboundDetail NO-LOCK
         WHERE bf-link-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-link-APIOutboundDetail.detailID      EQ "JobLink"
           AND bf-link-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
         NO-ERROR.
    IF AVAILABLE bf-link-APIOutboundDetail THEN 
    DO:        
        FOR EACH ttLink:
            lcJobLinkData = bf-link-APIOutboundDetail.data.

            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "ToPart", ttLink.toPart).
            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "FromPart", ttLink.fromPart).
            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "ToTask", ttLink.toTask).
            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "FromTask", ttLink.fromTask).
            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "Action", ttLink.action).
            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "ToLocation", ttLink.toLocation).
            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "FromLocation", ttLink.fromLocation).
            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "Form", STRING(ttLink.formNo)).
            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "Blank", STRING(ttLink.blankNo)).
            RUN updateRequestData(INPUT-OUTPUT lcJobLinkData, "Pass", STRING(ttLink.passNo)).
            
            ttlink.requestData = lcJobLinkData.   
        END.
    END. 
END PROCEDURE.

PROCEDURE pCreateMaterials PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job. 

    DEFINE BUFFER bf-job-mat-APIOutboundDetail FOR APIOutboundDetail.
    
    FIND FIRST bf-job-mat-APIOutboundDetail NO-LOCK
         WHERE bf-job-mat-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-job-mat-APIOutboundDetail.detailID      EQ "JobMaterial"
           AND bf-job-mat-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
         NO-ERROR. 

    FOR EACH job-mat NO-LOCK
        WHERE job-mat.company EQ ipbf-job.company
          AND job-mat.job     EQ ipbf-job.job
          AND job-mat.job-no  EQ ipbf-job.job-no
          AND job-mat.job-no2 EQ ipbf-job.job-no2
        USE-INDEX seq-idx:
            
        ASSIGN 
            cBoardCode = ""
            cCylinder  = ""
            cTray      = ""
            cGrain     = ""
            .
                    
        FOR EACH job-hdr 
            WHERE job-hdr.company EQ job-mat.company
              AND job-hdr.job     EQ job-mat.job
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
            IF LAST-OF(eb.form-no) THEN 
            DO:
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

        IF AVAILABLE bf-job-mat-APIOutboundDetail THEN
            lcJobMatData = bf-job-mat-APIOutboundDetail.data. 

        ASSIGN
            cItemNumber             = job-mat.i-no
            cJobMatItemStandardCost = TRIM(STRING(job-mat.std-cost,">>>,>>9.99<<"))
            cStandardCostUOM        = job-mat.sc-uom
            cCostPerUOM             = TRIM(STRING(job-mat.cost-m,"->,>>9.9999"))
            cQtyToOrder             = TRIM(STRING(job-mat.qty,">,>>>,>>9.9<<<<<"))
            cQuantityUOM            = job-mat.qty-uom
            cLength                 = TRIM(STRING(job-mat.len,">>9.99<<"))
            cWidth                  = TRIM(STRING(job-mat.wid,">>9.99<<"))
            cBasisWeight            = TRIM(STRING(job-mat.basis-w,">>9.99"))
            cLineNumber             = TRIM(STRING(job-mat.line,"99"))
            cRMItem                 = job-mat.rm-i-no
            cJobMatBlank            = TRIM(STRING(job-mat.blank-no,">9"))
            cJobMatForm             = TRIM(STRING(job-mat.frm,">>9"))
            cAllocated              = STRING(job-mat.all-flg)
            cQuantityAllocated      = TRIM(STRING(job-mat.qty-all,">>>,>>9.99<<<<"))
            cQuantityIssued         = TRIM(STRING(job-mat.qty-iss,"->>,>>9.99<<<<"))
            cMRQuantity             = TRIM(STRING(job-mat.qty-mr,">>>>9.99<<<<"))
            cWasteQuantity          = TRIM(STRING(job-mat.qty-wst,">>>>9.99<<<<"))
            cDepth                  = TRIM(STRING(job-mat.dep,">,>>9.99<<<<"))
            cPONumber               = TRIM(STRING(job-mat.po-no,">>>>>9"))
            cCrossGrain             = IF job-mat.xGrain = "N" THEN "NO" ELSE IF job-mat.xGrain = "S" THEN "(S)heet"
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
            
        CREATE ttMaterial.
        ASSIGN
            ttMaterial.formNo      = job-mat.frm
            ttMaterial.blankNo     = job-mat.blank-no
            ttMaterial.passNo      = job-mat.pass
            ttMaterial.requestData = lcJobMatData
            .                                              
    END.
END PROCEDURE.

PROCEDURE pUpdateCustInfo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomerID          AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
    
    DEFINE VARIABLE cCustomer     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerName AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-cust FOR cust.
    
    FIND FIRST bf-cust NO-LOCK
         WHERE bf-cust.company EQ ipcCompany
           AND bf-cust.cust-no EQ ipcCustomerID
         NO-ERROR.
    IF AVAILABLE bf-cust THEN
        ASSIGN
            cCustomer     = bf-cust.cust-no
            cCustomerName = bf-cust.name
            .

    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerID", cCustomer).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustomerName", cCustomerName).            
END PROCEDURE.

PROCEDURE pUpdateItemInfo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcItemID        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
    
    DEFINE VARIABLE cItem        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProductCode AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    FIND FIRST bf-itemfg NO-LOCK
         WHERE bf-itemfg.company EQ ipcCompany 
           AND bf-itemfg.i-no    EQ ipcItemID
         NO-ERROR.
    IF AVAILABLE bf-itemfg THEN
        ASSIGN
            cItem     = bf-itemfg.i-no
            cItemName = bf-itemfg.i-name
            cProductCode = bf-itemfg.procat
            .
            
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Item", cItem).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FGItemName", cItemName).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ProductCode", cProductCode).    
END PROCEDURE.

PROCEDURE pIsACombottPart PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiFormNo  AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsCombo AS LOGICAL NO-UNDO.
    
    oplIsCombo = CAN-FIND(FIRST ttPart
                          WHERE ttPart.formNo  EQ ipiFormNo
                            AND ttPart.blankNo EQ ipiBlankNo
                            AND ttPart.comboHdr).    
END PROCEDURE.

PROCEDURE pProcessAMSData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.    

    DEFINE VARIABLE lIsSet           AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lcPartData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcTaskData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatTaskData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE iTaskID          AS INTEGER  NO-UNDO.
    DEFINE VARIABLE lTaskAvailable   AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE iTaskCounter     AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iMaterialCount   AS INTEGER  NO-UNDO.
    
    DEFINE VARIABLE lIsNewCalculationMethod AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-ttMaterial FOR ttMaterial.
    
    lIsSet = CAN-FIND(FIRST est NO-LOCK
                      WHERE est.company EQ ipbf-job.company
                        AND est.est-no  EQ ipbf-job.est-no
                        AND (est.est-type EQ 2 OR est.est-type EQ 6)).

    lIsNewCalculationMethod = CAN-FIND(FIRST job-hdr
                                       WHERE job-hdr.company  EQ ipbf-job.company
                                         AND job-hdr.job      EQ ipbf-job.job
                                         AND job-hdr.job-no   EQ ipbf-job.job-no
                                         AND job-hdr.job-no2  EQ ipbf-job.job-no2
                                         AND job-hdr.frm      EQ 0
                                         AND job-hdr.blank-no EQ 0). 

    RUN pCreateParts(
        BUFFER ipbf-job,
        INPUT  lIsNewCalculationMethod
        ).
        
    RUN pCreateTasks(
        BUFFER ipbf-job
        ).

    RUN pCreateMaterials(
        BUFFER ipbf-job
        ).
    
    /* If a set, then find the last blank of last form */
    IF lIsSet THEN DO:
        FOR EACH ttTask:
            iTaskID = ttTask.taskID.
        END.
    END.
        
    /* Any non-combo forms should simply list the “Tasks” for all job-mch on that form, within the Part on that form. */
    FOR EACH ttPart
        WHERE NOT ttPart.comboPart
          AND NOT ttPart.setAssembly:        
        lTaskAvailable = FALSE.
        
        FOR EACH ttTask
            WHERE ttTask.formNo EQ ttPart.formNo:
            IF lIsSet AND ttTask.taskID EQ iTaskID THEN
                NEXT.
            
            ASSIGN
                ttTask.location = ttPart.location
                ttTask.partID   = ttPart.partID
                .
            
            ttPart.taskIDs = ttPart.taskIDs + "," + STRING(ttTask.taskID).
            
            lTaskAvailable = TRUE.
        END.
    
        ttPart.taskIDs = TRIM(ttPart.taskIDs, ",").
        
        IF NOT lTaskAvailable THEN
            DELETE ttPart.
    END.

    /* If there is a Combo Form (multiple blanks out of one sheet/form), we need to add a “Combo Form” 
       part that will contain all Sheet fed operations for the items on the form. 
       Job-mch records that are blank specific must be listed under the specific blank Part */
    FOR EACH ttPart
        WHERE ttPart.comboPart
          AND NOT ttPart.setAssembly:
        lTaskAvailable = FALSE.
        
        FOR EACH ttTask
            WHERE ttTask.formNo  EQ ttPart.formNo:
            IF lIsSet AND ttTask.taskID EQ iTaskID THEN
                NEXT.
                
            IF ttPart.comboHdr AND ttTask.blankNo NE 0 THEN
                NEXT.

            IF NOT ttPart.comboHdr AND ttTask.blankNo NE ttPart.blankNo THEN
                NEXT.
            
            ASSIGN
                ttTask.location = ttPart.location
                ttTask.partID   = ttPart.partID
                .
            
            ttPart.taskIDs = ttPart.taskIDs + "," + STRING(ttTask.taskID).
            
            lTaskAvailable = TRUE.
        END.

        ttPart.taskIDs = TRIM(ttPart.taskIDs, ",").

        IF NOT lTaskAvailable THEN
            DELETE ttPart.
    END.

    /* If the job is for a Set, we need to make the assumption that the last operation on the 
       last blank of the last form is an “Assembly” operation that ties all parts together.  
       We will need to identify the Set Header as a separate part and list this single 
       job-mch under that part (this is the one exception to rule #1). */
    IF NOT lIsSet THEN DO:
        FOR EACH ttPart
            WHERE ttPart.setAssembly:
            DELETE ttPart.
        END.
    END.

    FOR EACH ttPart
        WHERE ttPart.setAssembly:
        /* Code to find the last blank of last form */
        FIND FIRST ttTask
             WHERE ttTask.taskID EQ iTaskID
             NO-ERROR.
        IF AVAILABLE ttTask THEN
            ASSIGN
                ttTask.location = ttPart.location
                ttTask.partID   = ttPart.partID
                ttPart.taskIDs  = STRING(ttTask.taskID)
                .                    
    END.

    FOR EACH ttPart:
        IF ttPart.taskIDs EQ "" THEN
            NEXT.

        ASSIGN
            lcConcatTaskData = ""
            lcPartData       = ttPart.requestData
            lTaskAvailable   = FALSE
            .
        
        lcPartData = ttPart.requestData.
           
        DO iTaskCounter = 1 TO NUM-ENTRIES(ttPart.taskIDs):
            FIND FIRST ttTask
                 WHERE ttTask.taskID EQ INTEGER(ENTRY(iTaskCounter, ttPart.taskIDs))
                 NO-ERROR.
            IF AVAILABLE ttTask THEN DO:
                ASSIGN
                    ttTask.location  = ttPart.location
                    ttTask.partID    = ttPart.partID
                    lcTaskData       = ttTask.requestData            
                    .

                /* If die cut value existis and first task of blank and form */
                IF ttPart.dieInches NE 0 AND iTaskCounter EQ 1 THEN
                    ttTask.runQuantity = ttTask.runQuantity * ttPart.dieInches.

                RUN updateRequestData(INPUT-OUTPUT lcTaskData, "RunQuantityWithCountMultiplier", STRING(ttTask.runQuantity)).

                lcConcatTaskData = lcConcatTaskData + lcTaskData.                    
            END.
        END.

        lcPartData = REPLACE(lcPartData, "$JobMachine$", lcConcatTaskData).
        
        ttPart.requestData = lcPartData.        
    END.

    /* Code to add Part information in Materials */
    FOR EACH ttMaterial
        WHERE ttMaterial.partID EQ ""
        BY ttMaterial.formNo
        BY ttMaterial.blankNo:
        /* Link all the form 0 and blank 0 to assembly part */
        IF ttMaterial.formNo EQ 0 AND ttMaterial.blankNo EQ 0 THEN DO:
            FIND FIRST ttPart
                 WHERE ttPart.setAssembly
                 NO-ERROR.
            IF AVAILABLE ttPart THEN
                ttMaterial.partID = ttPart.partID.
            
            NEXT.
        END.    
        
        IF ttMaterial.blankNo EQ 0 THEN DO:             
            /* If blank 0 and the part is a combo, then create a materail for all the applicable combo parts */
            IF CAN-FIND(FIRST ttPart
                     WHERE ttPart.formNo EQ ttMaterial.formNo
                       AND ttPart.comboPart) THEN DO:
                FOR EACH ttPart
                    WHERE ttPart.formNo    EQ ttMaterial.formNo
                      AND ttPart.comboPart EQ TRUE
                      AND ttPart.comboHdr  EQ FALSE:
                    CREATE bf-ttMaterial.
                    BUFFER-COPY ttMaterial TO bf-ttMaterial.
                    
                    bf-ttMaterial.partID = ttPart.partID.
                END. 
                
                DELETE ttMaterial.
            
                NEXT.
            END.
            /* Link the 0 blank material to the part that matches it's form (Single) */
            ELSE DO:
                FIND FIRST ttPart
                     WHERE ttPart.formNo  EQ ttMaterial.formNo
                     NO-ERROR.
                IF AVAILABLE ttPart THEN DO:
                    ttMaterial.partID = ttPart.partID.
                    
                    NEXT. 
                END.          
            END.
        END.
        
        /* Link corresponding blank and form part */
        FIND FIRST ttPart
             WHERE ttPart.formNo  EQ ttMaterial.formNo
               AND ttPart.blankNo EQ ttMaterial.blankNo
             NO-ERROR.
        IF AVAILABLE ttPart THEN DO:
            ttMaterial.partID = ttPart.partID.
            
            NEXT.
        END.    
        
        /* If still there is no applicable part, it means that job is a set and
           the materail is for last blank of last form which is replaced by set assembly part */
           /* Reveiw required */
/*        FIND FIRST ttPart                     */
/*             WHERE ttPart.setAssembly         */
/*             NO-ERROR.                        */
/*        IF AVAILABLE ttPart THEN              */
/*            ttMaterial.partID = ttPart.partID.*/
    END.
    
    /* Apply the sequences and part to the request data */
    FOR EACH ttMaterial
        BY ttMaterial.formNo
        BY ttMaterial.blankNo:
        
        ASSIGN
            lcJobMatData      = ttMaterial.requestData
            iMaterialCount    = iMaterialCount + 1
            ttMaterial.lineID = iMaterialCount
            .
            
        RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "MaterialSequence", STRING(ttMaterial.lineID)).
        RUN updateRequestData(INPUT-OUTPUT lcJobMatData, "JobHeaderPart", ttMaterial.partID).        
        
        ttMaterial.requestData = lcJobMatData.
    END.
      
    RUN pCreateLinks (
        INPUT lIsSet
        ).    

END PROCEDURE.

PROCEDURE pCreateParts PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.    
    DEFINE INPUT  PARAMETER iplIsNewCalculationMethod AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-job-hdr-APIOutboundDetail FOR APIOutboundDetail.
    DEFINE BUFFER bf-combo-APIOutboundDetail   FOR APIOutboundDetail.
    DEFINE BUFFER bf-ef                        FOR ef.
    
    DEFINE VARIABLE lIsNewCalculationMethod AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lIsACombo               AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cForm                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlank                  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantity               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKeyItem                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNumberOn               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcJobHeaderData         AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcComboFormData         AS LONGCHAR  NO-UNDO.
                                            
    FIND FIRST bf-job-hdr-APIOutboundDetail NO-LOCK
         WHERE bf-job-hdr-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-job-hdr-APIOutboundDetail.detailID      EQ "JobHeader"
           AND bf-job-hdr-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
         NO-ERROR.
    IF NOT AVAILABLE bf-job-hdr-APIOutboundDetail THEN
        RETURN.
        
    FIND FIRST bf-combo-APIOutboundDetail NO-LOCK
         WHERE bf-combo-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-combo-APIOutboundDetail.detailID      EQ "ComboForm"
           AND bf-combo-APIOutboundDetail.parentID      EQ "JobHeader"
         NO-ERROR.
                                   
    IF NOT iplIsNewCalculationMethod THEN DO:                    
        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company  EQ ipbf-job.company
               AND job-hdr.job      EQ ipbf-job.job
               AND job-hdr.job-no   EQ ipbf-job.job-no
               AND job-hdr.job-no2  EQ ipbf-job.job-no2
               AND job-hdr.frm      EQ 1
               AND job-hdr.blank-no EQ 1
             NO-ERROR.   
        IF NOT AVAILABLE job-hdr THEN
            RETURN.
            
        FOR FIRST est NO-LOCK
            WHERE est.company EQ ipbf-job.company
              AND est.est-no  EQ ipbf-job.est-no,
            EACH ef NO-LOCK
            WHERE ef.company EQ est.company
              AND ef.est-no  EQ est.est-no,
            EACH eb NO-LOCK
            WHERE eb.company EQ ef.company
              AND eb.est-no  EQ ef.est-no
              AND eb.form-no EQ ef.form-no
            BREAK BY eb.form-no
                  BY eb.blank-no:           
            lIsACombo = FALSE.

            RUN pIsAComboEst (
                INPUT  eb.company,
                INPUT  eb.est-no,
                INPUT  eb.form-no,
                INPUT  eb.blank-no,
                OUTPUT lIsACombo
                ).
            
            ASSIGN
                cItem     = eb.stock-no
                cForm     = TRIM(STRING(eb.form-no,">>9"))
                cBlank    = TRIM(STRING(eb.blank-no,">9"))
                cQuantity = STRING(job-hdr.qty * eb.quantityperSet)
                cNumberOn = STRING(eb.num-up)
                .
            
            lcJobHeaderData = bf-job-hdr-APIOutboundDetail.data.
            
            RUN pUpdateItemInfo(INPUT job.company, INPUT cItem, INPUT-OUTPUT lcJobHeaderData).
            RUN pUpdateCustInfo(INPUT job.company, INPUT job-hdr.cust-no, INPUT-OUTPUT lcJobHeaderData).
            RUN pGetOrderQuantity(BUFFER job-hdr, OUTPUT dOrderQty).

            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Blank", cBlank).
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Form", cForm).
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "KeyItem", cKeyItem). 
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "NumberOn", cNumberOn).                    
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Quantity",cQuantity).
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "QuantityOrdered",STRING(dOrderQty)).
            
            CREATE ttPart.
            ASSIGN
                ttPart.formNo      = eb.form-no
                ttPart.blankNo     = eb.blank-no
                ttPart.partID      = eb.stock-no
                ttPart.location    = eb.loc
                ttPart.dieInches   = ef.die-in
                ttPart.requestData = lcJobHeaderData
                ttPart.comboPart   = lIsACombo
                .

            IF LAST-OF(eb.form-no) THEN DO:
                IF lIsACombo AND AVAILABLE bf-combo-APIOutboundDetail THEN DO:
                    lcComboFormData = bf-combo-APIOutboundDetail.data.

                    RUN updateRequestData(INPUT-OUTPUT lcComboFormData, "Blank", cBlank).
                    RUN updateRequestData(INPUT-OUTPUT lcComboFormData, "Form", cForm).
                    
                    CREATE ttPart.
                    ASSIGN
                        ttPart.formNo      = eb.form-no
                        ttPart.blankNo     = 0
                        ttPart.partID      = "Combo " + cForm
                        ttPart.location    = eb.loc
                        ttPart.requestData = lcComboFormData
                        ttPart.comboHdr    = TRUE
                        ttPart.comboPart   = lIsACombo
                        .                    
                END.
            END.
        END.

        /* Header Part */
        ASSIGN 
            cItem     = job-hdr.i-no
            cForm     = TRIM(STRING(job-hdr.frm,">>9"))
            cBlank    = TRIM(STRING(job-hdr.blank-no,">9"))
            cQuantity = STRING(job-hdr.qty)
            cKeyItem  = STRING(INTEGER(job-hdr.keyItem))
            cNumberOn = STRING(job-hdr.n-on)
            .
        
        lcJobHeaderData = bf-job-hdr-APIOutboundDetail.data.
        
        RUN pUpdateItemInfo(INPUT job.company, INPUT cItem, INPUT-OUTPUT lcJobHeaderData).
        RUN pUpdateCustInfo(INPUT job.company, INPUT job-hdr.cust-no, INPUT-OUTPUT lcJobHeaderData).
        RUN pGetOrderQuantity(BUFFER job-hdr, OUTPUT dOrderQty).
            
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Blank", cBlank).
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Form", cForm).
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "KeyItem", cKeyItem). 
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "NumberOn", cNumberOn).   
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Quantity",cQuantity).
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "OrderQuantity",STRING(dOrderQty)).
                        
        CREATE ttPart.
        ASSIGN
            ttPart.formNo      = job-hdr.frm
            ttPart.blankNo     = job-hdr.blank-no
            ttPart.partID      = job-hdr.i-no
            ttPart.location    = job-hdr.loc
            ttPart.requestData = lcJobHeaderData
            ttPart.setAssembly = TRUE
            .
    END.   
    ELSE DO:
        FOR EACH job-hdr 
            WHERE job-hdr.company  EQ ipbf-job.company
              AND job-hdr.job      EQ ipbf-job.job
              AND job-hdr.job-no   EQ ipbf-job.job-no
              AND job-hdr.job-no2  EQ ipbf-job.job-no2
              AND job-hdr.frm      NE 0
              AND job-hdr.blank-no NE 0
            BREAK BY job-hdr.frm
                  BY job-hdr.blank-no:
            lIsACombo = FALSE.

            FIND FIRST bf-ef NO-LOCK
                 WHERE bf-ef.company EQ ipbf-job.company
                   AND bf-ef.est-no  EQ ipbf-job.est-no
                   AND bf-ef.form-no EQ job-hdr.frm
                 NO-ERROR.
                 
            RUN pIsAComboJob (
                BUFFER  job-hdr,
                OUTPUT lIsACombo
                ).
            
            ASSIGN
                cItem     = job-hdr.i-no
                cForm     = TRIM(STRING(job-hdr.frm,">>9"))
                cBlank    = TRIM(STRING(job-hdr.blank-no,">9"))
                cQuantity = STRING(job-hdr.qty)
                cNumberOn = STRING(job-hdr.n-on)
                .
            
            lcJobHeaderData = bf-job-hdr-APIOutboundDetail.data.
            
            RUN pUpdateItemInfo(INPUT job.company, INPUT cItem, INPUT-OUTPUT lcJobHeaderData).
            RUN pUpdateCustInfo(INPUT job.company, INPUT job-hdr.cust-no, INPUT-OUTPUT lcJobHeaderData).
            RUN pGetOrderQuantity(BUFFER job-hdr, OUTPUT dOrderQty).
                
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Blank", cBlank).
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Form", cForm).
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "KeyItem", cKeyItem). 
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "NumberOn", cNumberOn).                    
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Quantity",cQuantity).
            RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "OrderQuantity",STRING(dOrderQty)).
                
            CREATE ttPart.
            ASSIGN
                ttPart.formNo      = job-hdr.frm
                ttPart.blankNo     = job-hdr.blank-no
                ttPart.partID      = job-hdr.i-no
                ttPart.location    = job-hdr.loc
                ttPart.requestData = lcJobHeaderData
                ttPart.comboPart   = lIsACombo
                .
            
            IF AVAILABLE bf-ef THEN
                ttPart.dieInches = bf-ef.die-in.
                
            IF LAST-OF(job-hdr.frm) THEN DO:
                IF lIsACombo AND AVAILABLE bf-combo-APIOutboundDetail THEN DO:
                    lcComboFormData = bf-combo-APIOutboundDetail.data.

                    RUN updateRequestData(INPUT-OUTPUT lcComboFormData, "Blank", cBlank).
                    RUN updateRequestData(INPUT-OUTPUT lcComboFormData, "Form", cForm).
                    
                    CREATE ttPart.
                    ASSIGN
                        ttPart.formNo      = job-hdr.frm
                        ttPart.blankNo     = 0
                        ttPart.partID      = "Combo " + cForm
                        ttPart.location    = job-hdr.loc
                        ttPart.requestData = lcComboFormData
                        ttPart.comboHdr    = TRUE
                        ttPart.comboPart   = lIsACombo
                        .                    
                END.
            END.
        END.

        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company  EQ ipbf-job.company
               AND job-hdr.job      EQ ipbf-job.job
               AND job-hdr.job-no   EQ ipbf-job.job-no
               AND job-hdr.job-no2  EQ ipbf-job.job-no2
               AND job-hdr.frm      EQ 0
               AND job-hdr.blank-no EQ 0
             NO-ERROR.   
        IF NOT AVAILABLE job-hdr THEN
            RETURN.
            
        /* Header Part */
        ASSIGN 
            cItem     = job-hdr.i-no
            cForm     = TRIM(STRING(job-hdr.frm,">>9"))
            cBlank    = TRIM(STRING(job-hdr.blank-no,">9"))
            cQuantity = STRING(job-hdr.qty)
            cKeyItem  = STRING(INTEGER(job-hdr.keyItem))
            cNumberOn = STRING(job-hdr.n-on)
            .
        
        lcJobHeaderData = bf-job-hdr-APIOutboundDetail.data.

        RUN pUpdateItemInfo(INPUT job.company, INPUT cItem, INPUT-OUTPUT lcJobHeaderData).
        RUN pUpdateCustInfo(INPUT job.company, INPUT job-hdr.cust-no, INPUT-OUTPUT lcJobHeaderData).
        RUN pGetOrderQuantity(BUFFER job-hdr, OUTPUT dOrderQty).
            
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Blank", cBlank).
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Form", cForm).
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "KeyItem", cKeyItem). 
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "NumberOn", cNumberOn).   
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "Quantity",cQuantity).
        RUN updateRequestData(INPUT-OUTPUT lcJobHeaderData, "OrderQuantity",STRING(dOrderQty)).
                    
        CREATE ttPart.
        ASSIGN
            ttPart.formNo      = job-hdr.frm
            ttPart.blankNo     = job-hdr.blank-no
            ttPart.partID      = job-hdr.i-no
            ttPart.location    = job-hdr.loc
            ttPart.requestData = lcJobHeaderData
            ttPart.setAssembly = TRUE
            .    
    END.
    
END PROCEDURE.

PROCEDURE pCreateTasks PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.

    DEFINE VARIABLE lcJobMachineData AS LONGCHAR NO-UNDO.
    
    DEFINE BUFFER bf-job-mch-APIOutboundDetail FOR APIOutboundDetail.
    
    FIND FIRST bf-job-mch-APIOutboundDetail NO-LOCK
         WHERE bf-job-mch-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-job-mch-APIOutboundDetail.detailID      EQ "JobMachine"
           AND bf-job-mch-APIOutboundDetail.parentID      EQ "JobHeader"
         NO-ERROR.

    FOR EACH job-mch NO-LOCK
        WHERE job-mch.company   EQ ipbf-job.company
          AND job-mch.job       EQ ipbf-job.job
          AND job-mch.job-no    EQ ipbf-job.job-no
          AND job-mch.job-no2   EQ ipbf-job.job-no2
        USE-INDEX line-idx:
        IF AVAILABLE bf-job-mch-APIOutboundDetail THEN
            lcJobMachineData = bf-job-mch-APIOutboundDetail.data.
        
        CREATE ttTask.
        ASSIGN
            ttTask.formNo      = job-mch.frm
            ttTask.blankNo     = job-mch.blank-no
            ttTask.lineID      = job-mch.line
            ttTask.passNo      = job-mch.pass
            ttTask.taskID      = job-mch.job-mchID
            ttTask.runQuantity = job-mch.run-qty
            .

        RUN pUpdateMachineDetails(
            BUFFER job-mch,
            INPUT  lcJobMachineData,
            OUTPUT lcJobMachineData
            ).

        ttTask.requestData = lcJobMachineData.               
    END.
END PROCEDURE.

PROCEDURE pIsAComboEst PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimate AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo  AS INTEGER   NO-UNDO.
    
    DEFINE OUTPUT PARAMETER oplCombo AS LOGICAL NO-UNDO.
    
    oplCombo = CAN-FIND(FIRST eb
                        WHERE eb.company  EQ ipcCompany
                          AND eb.est-no   EQ ipcEstimate
                          AND eb.form-no  EQ ipiFormNo
                          AND eb.blank-no NE ipiBlankNo).

END PROCEDURE.

PROCEDURE pIsAComboJob PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-hdr FOR job-hdr.
    DEFINE OUTPUT PARAMETER oplCombo AS LOGICAL NO-UNDO.
    
    oplCombo = CAN-FIND(FIRST job-hdr
                        WHERE job-hdr.company  EQ ipbf-job-hdr.company
                          AND job-hdr.job      EQ ipbf-job-hdr.job
                          AND job-hdr.job-no   EQ ipbf-job-hdr.job-no
                          AND job-hdr.job-no2  EQ ipbf-job-hdr.job-no2
                          AND job-hdr.frm      EQ ipbf-job-hdr.frm
                          AND job-hdr.blank-no NE ipbf-job-hdr.blank-no).

END PROCEDURE.

PROCEDURE pUpdateMachineDetails PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-mch FOR job-mch.
    DEFINE INPUT  PARAMETER iplcJobMachineData             AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcConcatJobMachineDataByItem AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE lcJobMachineDataByItem        AS LONGCHAR  NO-UNDO.
    
    DEFINE BUFFER bf-mach FOR mach.
    
    IF NOT AVAILABLE ipbf-job-mch THEN
        RETURN.

    ASSIGN
        lcJobMachineDataByItem     = iplcJobMachineData
        cAnchored                  = STRING(ipbf-job-mch.anchored)
        cMachineCode               = ipbf-job-mch.m-code
        cMRWaste                   = TRIM(STRING(ipbf-job-mch.mr-waste,">>>9"))
        cMRRate                    = TRIM(STRING(ipbf-job-mch.mr-rate,">>9.99"))
        cMchMRvariableOverHeadRate = TRIM(STRING(ipbf-job-mch.mr-varoh,">>9.99"))
        cMchMRFixedOverheadRate    = TRIM(STRING(ipbf-job-mch.mr-fixoh,">>9.99"))
        cDepartment                = ipbf-job-mch.dept
        cItemName                  = ipbf-job-mch.i-name
        cJobMchItem                = ipbf-job-mch.i-no
        cJobMchForm                = TRIM(STRING(ipbf-job-mch.frm,">>9"))
        cRunHours                  = TRIM(STRING(ipbf-job-mch.run-hr,">>9.99"))
        cRunMinutes                = TRIM(STRING(ROUND(ipbf-job-mch.run-hr * 60, 0)))
        cRunSpeed                  = TRIM(STRING(ipbf-job-mch.speed, ">>>>9"))
        cMRHours                   = TRIM(STRING(ipbf-job-mch.mr-hr,">>9.99"))
        cMRMinutes                 = TRIM(STRING(ROUND(ipbf-job-mch.mr-hr * 60, 0)))
        cJobMchBlank               = TRIM(STRING(ipbf-job-mch.blank-no,">9"))
        cJobMchLineNumber          = TRIM(STRING(ipbf-job-mch.line,">9"))
        cRunQuantity               = TRIM(STRING(ipbf-job-mch.run-qty,">,>>>,>>9.9<<"))
        cRunStartDate              = TRIM(STRING(ipbf-job-mch.start-date,"99/99/9999"))
        cRunStartTime              = TRIM(STRING(ipbf-job-mch.start-time,"->,>>>,>>9"))
        cQueueTime                 = TRIM(STRING(ipbf-job-mch.queue-time,">>>>9"))
        cLagTime                   = TRIM(STRING(ipbf-job-mch.lag-time,">>>>9"))
        cEndDate                   = STRING(ipbf-job-mch.end-date,"99/99/9999")
        cEndTime                   = TRIM(STRING(ipbf-job-mch.end-time,"->,>>>,>>9"))
        cSetupStartDate            = TRIM(STRING(ipbf-job-mch.start-date-su,"99/99/9999"))
        cSetupStartTime            = TRIM(STRING(ipbf-job-mch.start-time-su,"->,>>>,>>9"))
        cSetupEndDate              = STRING(ipbf-job-mch.end-date-su,"99/99/9999")
        cSetupEndTime              = TRIM(STRING(ipbf-job-mch.end-time-su,"->,>>>,>>9"))
        cMRComplete                = STRING(ipbf-job-mch.mr-complete)
        cRunComplete               = STRING(ipbf-job-mch.run-complete)
        cJobMchDueDate             = STRING(ipbf-job-mch.due-date,"99/99/9999")
        cJobMchDueTime             = TRIM(STRING(ipbf-job-mch.due-time,">>>>9"))
        cMRTotalRate               = TRIM(STRING(ipbf-job-mch.mr-trate,">>9.99"))
        cFixedOverHeadRate         = TRIM(STRING(ipbf-job-mch.run-fixoh,">>9.99"))
        cRunProfitPct              = TRIM(STRING(ipbf-job-mch.run-profit,">>9.99"))
        cMRProfitPct               = TRIM(STRING(ipbf-job-mch.mr-profit,">>9.99"))
        cRunRate                   = TRIM(STRING(ipbf-job-mch.run-rate,">>9.99"))
        cRunTotalRate              = TRIM(STRING(ipbf-job-mch.run-trate,">>9.99"))
        cVariableOverheadRate      = TRIM(STRING(ipbf-job-mch.run-varoh,">>9.99"))
        cMRContributionRate        = TRIM(STRING(ipbf-job-mch.mr-cont,">>9.99"))
        cRunContributionRate       = TRIM(STRING(ipbf-job-mch.run-cont,">>9.99"))
        cJobMachineID              = TRIM(STRING(ipbf-job-mch.job-mchID,">>>>>>9"))
        cJobMchWastePct            = TRIM(STRING(ipbf-job-mch.wst-prct,">>9.99"))
        . 

    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Anchored",cAnchored).   
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Blank",cJobMchBlank).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Department",cDepartment).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "DueDate",cJobMchDueDate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "DueTime",cJobMchDueTime).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunEndDate",cEndDate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "SetupEndDate",cSetupEndDate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunEndTime",cEndTime).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "SetupEndtime",cSetupEndTime).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Form",cJobMchForm).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "ItemName",cItemName).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Item",cJobMchItem).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MachineID",cJobMachineID).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "LagTime",cLagTime).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Line",cJobMchLineNumber).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MachineCode",cMachineCode).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MRContributionRate",cMRContributionRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MRFixedOHRate",cMchMRFixedOverheadRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MRRate",cMRRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MRTotalRate",cMRTotalRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MRVariableOverHeadRate",cMchMRvariableOverHeadRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MRWaste",cMRWaste).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "QueueTime",cQueueTime).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunContributionRate",cRunContributionRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "FixedOverHeadRate",cFixedOverHeadRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunHours",cRunHours).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunMinutes",cRunMinutes).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MakeReadyHours",cMRHours).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MakeReadyMinutes",cMRMinutes).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunQuantity",cRunQuantity).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunRate",cRunRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunVarOH",cVariableOverheadRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunTotalRate",cRunTotalRate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunSpeed",cRunSpeed).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunStartDate",cRunStartDate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "SetupStartDate",cSetupStartDate).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunStartTime",cRunStartTime).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "SetupStartTime",cSetupStartTime).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunProfitPercentage",cRunProfitPct).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MRProfitPercentage",cMRProfitPct).
    RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "WastePercentage",cJobMchWastePct).
    
    FIND FIRST bf-mach NO-LOCK
         WHERE bf-mach.company EQ ipbf-job-mch.company
           AND bf-mach.m-code  EQ ipbf-job-mch.m-code
         NO-ERROR.
    IF AVAILABLE bf-mach THEN DO:
        RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MachineCodeSchedule", IF bf-mach.sch-m-code NE "" THEN bf-mach.sch-m-code ELSE cMachineCode).
        RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MachineIndustry", bf-mach.industry).
        RUN updateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MachineDescription", bf-mach.m-dscr).
    END.
    
    oplcConcatJobMachineDataByItem = oplcConcatJobMachineDataByItem + lcJobMachineDataByItem.         
END PROCEDURE.   

PROCEDURE pGetOrderQuantity PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-hdr FOR job-hdr.
    DEFINE OUTPUT PARAMETER opdOrderQuantity AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    FIND FIRST bf-oe-ord NO-LOCK 
         WHERE bf-oe-ord.company EQ ipbf-job-hdr.company 
           AND bf-oe-ord.ord-no  EQ ipbf-job-hdr.ord-no
         NO-ERROR.
    IF AVAILABLE bf-oe-ord THEN 
    DO:
        FIND bf-oe-ordl NO-LOCK 
             WHERE bf-oe-ordl.company EQ bf-oe-ord.company 
               AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no 
               AND bf-oe-ordl.i-no    EQ ipbf-job-hdr.i-no 
               AND bf-oe-ordl.job-no  EQ ipbf-job-hdr.job-no 
               AND bf-oe-ordl.job-no2 EQ ipbf-job-hdr.job-no2
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-ordl THEN 
            FIND bf-oe-ordl NO-LOCK 
                 WHERE bf-oe-ordl.company EQ bf-oe-ord.company 
                   AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
                   AND bf-oe-ordl.i-no    EQ ipbf-job-hdr.i-no 
                   AND bf-oe-ordl.job-no  EQ ipbf-job-hdr.job-no
                 NO-ERROR.
    END.
        
    IF AVAILABLE bf-oe-ordl THEN
        opdOrderQuantity = bf-oe-ordl.qty.
    ELSE
        opdOrderQuantity = ipbf-job-hdr.qty. 
END PROCEDURE.
