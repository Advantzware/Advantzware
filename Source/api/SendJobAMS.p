
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
    FIELD caliper      AS DECIMAL
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
DEFINE VARIABLE cNotes                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobStatus                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobDueDate                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFirstHeaderQuantity          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFirstHeaderCustomerID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFirstHeaderCustomerName      AS CHARACTER NO-UNDO.
    
/* Job Header Variables*/
DEFINE VARIABLE iTaskCounter                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLinkCounter                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lIsSet                        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lIsSetHeader                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iHeaderLink                   AS INTEGER   NO-UNDO.                
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
DEFINE VARIABLE dOrderQty                     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cMfgDate                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPriority                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIsPriority                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKeyItem                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNumberOn                     AS CHARACTER NO-UNDO.
    
/*Job Material variables*/
    
DEFINE VARIABLE cCrossGrain                   AS CHARACTER NO-UNDO.
    
/* Job Preps Variables */

DEFINE VARIABLE cSIMON                        AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE cAction                       AS CHARACTER NO-UNDO.

DEFINE VARIABLE dBoardLength    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dBoardWidth     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dJobBoardIssued AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cBoard          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInk            AS CHARACTER NO-UNDO.
DEFINE VARIABLE dInkQty         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cPallet         AS CHARACTER NO-UNDO.
DEFINE VARIABLE dPalletMRP      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cMatType5       AS CHARACTER NO-UNDO.
DEFINE VARIABLE dMatType5Qty    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMatType6       AS CHARACTER NO-UNDO.
DEFINE VARIABLE dMatType6Qty    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarnish        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAdders         AS CHARACTER NO-UNDO.
DEFINE VARIABLE dNoCases        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cCasesName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilmName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE dRequiredQty    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTotalMRP       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dMatLength      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iNumUp          AS INTEGER   NO-UNDO.      
DEFINE VARIABLE kFrac           AS DECIMAL   NO-UNDO.

DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.
                    
DEFINE BUFFER bf-APIOutboundDetail        FOR APIOutboundDetail.
DEFINE BUFFER bf-job-mat                  FOR job-mat.
DEFINE BUFFER bf-notes                    FOR notes.
DEFINE BUFFER bf-oe-ord                   FOR oe-ord.
/**********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

{api/SendJob.i}

/* **********************  Internal Procedures  *********************** */

RUN pSetRequestDataType (INPUT "XML").
oAttribute = NEW system.Attribute().
oAttribute:RequestDataType = gcRequestDataType.    

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
        cJobStatus  = IF job.stat EQ "C" OR job.stat EQ "Z" THEN "Closed"
                        ELSE IF job.stat EQ "H" THEN "OnHold"
                        ELSE "ProductionReady"
        cJobDueDate = IF job.due-date NE ? THEN STRING(job.due-date) ELSE "12/31/2049"
        .

    FIND FIRST bf-oe-ord NO-LOCK
         WHERE bf-oe-ord.company EQ job.company
           AND bf-oe-ord.job-no  EQ job.job-no
           AND bf-oe-ord.job-no2 EQ job.job-no2 
         NO-ERROR.
    
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
                lcJobPrepData = bf-APIOutboundDetail.data 
                cSIMON        = IF job-prep.simon EQ "S"      THEN "Seperate" 
                                ELSE IF job-prep.simon EQ "I" THEN "Integrate"
                                ELSE IF job-prep.simon EQ "M" THEN "Maintenance" 
                                ELSE IF job-prep.simon EQ "O" THEN "Other"
                                ELSE IF job-prep.simon EQ "N" THEN "No Charge"
                                ELSE job-prep.simon
                .  

            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobPrepData, "SIMON", cSIMON).
            lcJobPrepData = oAttribute:ReplaceAttributes(lcJobPrepData, BUFFER job-prep:HANDLE).
                            
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

    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company  EQ job.company
           AND job-hdr.job      EQ job.job
           AND job-hdr.job-no   EQ job.job-no
           AND job-hdr.job-no2  EQ job.job-no2
         NO-ERROR.
    IF AVAILABLE job-hdr THEN
        ASSIGN
            cFirstHeaderQuantity   = STRING(job-hdr.qty)
            cFirstHeaderCustomerID = job-hdr.cust-no
            .
    
    IF cFirstHeaderCustomerID NE "" THEN DO:
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ job.company
               AND cust.cust-no EQ cFirstHeaderCustomerID
             NO-ERROR.
        IF AVAILABLE cust THEN
            cFirstHeaderCustomerName = cust.name.
    END.
        
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobsData, "Notes", cNotes).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobsData, "FirstHeaderQuantity", cFirstHeaderQuantity).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobsData, "FirstHeaderCustomerID", cFirstHeaderCustomerID).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobsData, "FirstHeaderCustomerName", cFirstHeaderCustomerName).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobsData, "JobDueDate", cJobDueDate).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobsData, "JobStatus", cJobStatus).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobsData, "IsPriority", STRING(job.priority EQ 1,"true/false")).
    lcJobsData = oAttribute:ReplaceAttributes(lcJobsData, BUFFER job:HANDLE).
    lcJobsData = oAttribute:ReplaceAttributes(lcJobsData, BUFFER oe-ord:HANDLE).
    
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
                      AND INDEX("BPR",item.mat-type) GT 0:
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMatData, "BoardCode", item.i-name).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMatData, "Grain", ef.xgrain).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMatData, "Cylinder", STRING(ef.gsh-len)).      
                END.
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMatData, "Tray", eb.layer-pad).      
            END. 
        END.

        IF AVAILABLE bf-job-mat-APIOutboundDetail THEN
            lcJobMatData = bf-job-mat-APIOutboundDetail.data. 

        cCrossGrain = IF job-mat.xGrain = "N" THEN "NO" 
                      ELSE IF job-mat.xGrain = "S" THEN "(S)heet"
                      ELSE IF job-mat.xGrain = "B" THEN "(B)lank" 
                      ELSE job-mat.xgrain.
        

        cAction = system.SharedConfig:Instance:ConsumeValue(STRING(ROWID(job-mat))).
        IF cAction EQ "" THEN
            cAction = "Create".
            
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMatData, "CrossGrain", cCrossGrain).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMatData, "JobMaterialAction", cAction).
        lcJobMatData = oAttribute:ReplaceAttributes(lcJobMatData, BUFFER job-mat:HANDLE).

        RUN pUpdateRMItemInfo (job-mat.company, job-mat.i-no, INPUT-OUTPUT lcJobMatData).
                        
        CREATE ttMaterial.
        ASSIGN
            ttMaterial.formNo      = job-mat.frm
            ttMaterial.blankNo     = job-mat.blank-no
            ttMaterial.passNo      = job-mat.pass
            ttMaterial.requestData = lcJobMatData
            .                                              
    END.
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
        INPUT  lIsSet,
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
        /* Commenting the code to delete the parts without task, as AMS now allows us to send parts without a task */
/*        IF NOT lTaskAvailable THEN*/
/*            DELETE ttPart.        */
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
        /* Commenting the code to delete the parts without task, as AMS now allows us to send parts without a task */
/*        IF NOT lTaskAvailable THEN*/
/*            DELETE ttPart.        */
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
                RUN updateRequestData(INPUT-OUTPUT lcTaskData, "Caliper", STRING(ttPart.caliper)).
                RUN updateRequestData(INPUT-OUTPUT lcTaskData, "DieInches", STRING(ttPart.dieInches)).
                
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
    DEFINE INPUT  PARAMETER iplIsSet                  AS LOGICAL NO-UNDO.  
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
    DEFINE VARIABLE cLock                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantityReceived       AS DECIMAL   NO-UNDO.
    
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
                                   
    IF NOT iplIsNewCalculationMethod AND iplIsSet THEN DO:                    
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
                cForm     = STRING(eb.form-no)
                cBlank    = STRING(eb.blank-no)
                cQuantity = STRING(job-hdr.qty * (IF eb.quantityperSet EQ 0 THEN 1 ELSE eb.quantityperSet))
                cKeyItem  = STRING(INTEGER(job-hdr.keyItem))
                cNumberOn = STRING(eb.num-up)
                cLock     = STRING(job-hdr.lock, "TRUE/FALSE")
                .
            
            RUN fg/GetProductionQty.p (
                INPUT  job-hdr.company,
                INPUT  job-hdr.job-no,
                INPUT  job-hdr.job-no2,
                INPUT  job-hdr.i-no,
                INPUT  NO,
                OUTPUT dQuantityReceived
                ).
                                
            lcJobHeaderData = bf-job-hdr-APIOutboundDetail.data.

            cAction = system.SharedConfig:Instance:ConsumeValue(STRING(ROWID(job-hdr))).
            IF cAction EQ "" THEN
                cAction = "Create".
                            
            RUN pUpdateFGItemInfo(INPUT job.company, INPUT cItem, INPUT-OUTPUT lcJobHeaderData).
            RUN pUpdateCustomerInfo(INPUT job.company, INPUT job-hdr.cust-no, INPUT-OUTPUT lcJobHeaderData).
            RUN pGetOrderQuantity(BUFFER job-hdr, OUTPUT dOrderQty).

            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "Blank", cBlank).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "Form", cForm).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "KeyItem", cKeyItem). 
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "NumberOn", cNumberOn).                    
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "Quantity",cQuantity).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "QuantityOrdered",STRING(dOrderQty)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "IsJobHeaderLocked",cLock).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "QuantityReceived",STRING(dQuantityReceived)).           
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "JobHeaderAction", cAction).
            
            CREATE ttPart.
            ASSIGN
                ttPart.formNo      = eb.form-no
                ttPart.blankNo     = eb.blank-no
                ttPart.partID      = eb.stock-no
                ttPart.location    = eb.loc
                ttPart.dieInches   = ef.die-in
                ttPart.caliper     = ef.cal
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
            cForm     = STRING(job-hdr.frm)
            cBlank    = STRING(job-hdr.blank-no)
            cQuantity = STRING(job-hdr.qty)
            cKeyItem  = STRING(INTEGER(job-hdr.keyItem))
            cNumberOn = STRING(job-hdr.n-on)
            cLock     = STRING(job-hdr.lock, "TRUE/FALSE")
            .

        RUN fg/GetProductionQty.p (
            INPUT  job-hdr.company,
            INPUT  job-hdr.job-no,
            INPUT  job-hdr.job-no2,
            INPUT  job-hdr.i-no,
            INPUT  NO,
            OUTPUT dQuantityReceived
            ).
                    
        lcJobHeaderData = bf-job-hdr-APIOutboundDetail.data.
        
        RUN pUpdateFGItemInfo(INPUT job.company, INPUT cItem, INPUT-OUTPUT lcJobHeaderData).
        RUN pUpdateCustomerInfo(INPUT job.company, INPUT job-hdr.cust-no, INPUT-OUTPUT lcJobHeaderData).
        RUN pGetOrderQuantity(BUFFER job-hdr, OUTPUT dOrderQty).
            
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "KeyItem", cKeyItem). 
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "QuantityOrdered",STRING(dOrderQty)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "IsJobHeaderLocked",cLock).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "QuantityReceived",STRING(dQuantityReceived)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "JobHeaderAction", cAction).

        lcJobHeaderData = oAttribute:ReplaceAttributes(lcJobHeaderData, BUFFER job-hdr:HANDLE).
                                
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
                cForm     = STRING(job-hdr.frm)
                cBlank    = STRING(job-hdr.blank-no)
                cQuantity = STRING(job-hdr.qty)
                cKeyItem  = STRING(INTEGER(job-hdr.keyItem))
                cNumberOn = STRING(job-hdr.n-on)
                cLock     = STRING(job-hdr.lock, "TRUE/FALSE")
                .

            RUN fg/GetProductionQty.p (
                INPUT  job-hdr.company,
                INPUT  job-hdr.job-no,
                INPUT  job-hdr.job-no2,
                INPUT  job-hdr.i-no,
                INPUT  NO,
                OUTPUT dQuantityReceived
                ).
                            
            lcJobHeaderData = bf-job-hdr-APIOutboundDetail.data.

            cAction = system.SharedConfig:Instance:ConsumeValue(STRING(ROWID(job-hdr))).
            IF cAction EQ "" THEN
                cAction = "Create".
                            
            RUN pUpdateFGItemInfo(INPUT job.company, INPUT cItem, INPUT-OUTPUT lcJobHeaderData).
            RUN pUpdateCustomerInfo(INPUT job.company, INPUT job-hdr.cust-no, INPUT-OUTPUT lcJobHeaderData).
            RUN pGetOrderQuantity(BUFFER job-hdr, OUTPUT dOrderQty).
                
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "KeyItem", cKeyItem). 
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "QuantityOrdered",STRING(dOrderQty)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "IsJobHeaderLocked",cLock).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "QuantityReceived",STRING(dQuantityReceived)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "JobHeaderAction", cAction).
    
            lcJobHeaderData = oAttribute:ReplaceAttributes(lcJobHeaderData, BUFFER job-hdr:HANDLE).
            
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
                ASSIGN
                    ttPart.dieInches = bf-ef.die-in
                    ttPart.caliper   = bf-ef.cal
                    .
                
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
            cForm     = STRING(job-hdr.frm)
            cBlank    = STRING(job-hdr.blank-no)
            cQuantity = STRING(job-hdr.qty)
            cKeyItem  = STRING(INTEGER(job-hdr.keyItem))
            cNumberOn = STRING(job-hdr.n-on)
            cLock     = STRING(job-hdr.lock, "TRUE/FALSE")
            .

        RUN fg/GetProductionQty.p (
            INPUT  job-hdr.company,
            INPUT  job-hdr.job-no,
            INPUT  job-hdr.job-no2,
            INPUT  job-hdr.i-no,
            INPUT  NO,
            OUTPUT dQuantityReceived
            ).
                        
        lcJobHeaderData = bf-job-hdr-APIOutboundDetail.data.

        cAction = system.SharedConfig:Instance:ConsumeValue(STRING(ROWID(job-hdr))).
        IF cAction EQ "" THEN
            cAction = "Create".
                
        RUN pUpdateFGItemInfo(INPUT job.company, INPUT cItem, INPUT-OUTPUT lcJobHeaderData).
        RUN pUpdateCustomerInfo(INPUT job.company, INPUT job-hdr.cust-no, INPUT-OUTPUT lcJobHeaderData).
        RUN pGetOrderQuantity(BUFFER job-hdr, OUTPUT dOrderQty).
            
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "KeyItem", cKeyItem). 
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "QuantityOrdered",STRING(dOrderQty)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "IsJobHeaderLocked",cLock).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "QuantityReceived",STRING(dQuantityReceived)).
        oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobHeaderData, "JobHeaderAction", cAction).

        lcJobHeaderData = oAttribute:ReplaceAttributes(lcJobHeaderData, BUFFER job-hdr:HANDLE).
                                        
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

    DEFINE VARIABLE lcJobMachineData AS LONGCHAR  NO-UNDO.
        
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

        
        cAction = system.SharedConfig:Instance:ConsumeValue(STRING(ROWID(job-mch))).
        IF cAction EQ "" THEN
            cAction = "Create".
                    
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
        
        RUN updateRequestData(INPUT-OUTPUT lcJobMachineData, "JobMachineAction", cAction).   
         
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

    DEFINE VARIABLE lcJobMachineDataByItem AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE iQuantityWastedMR      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQuantityWastedRun     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQuantityWasted        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQuantityRun           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dHoursMR               AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dHoursRun              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lComplete              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lStartedMR             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lStartedRun            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lStarted               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMachineCodeSchedule   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMachineIndustry       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMachineDescription    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMRComplete            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRunComplete           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRunMinutes            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMRMinutes             AS CHARACTER NO-UNDO.
            
    DEFINE BUFFER bf-mach    FOR mach.
    DEFINE BUFFER bf-mch-act FOR mch-act.
    
    IF NOT AVAILABLE ipbf-job-mch THEN
        RETURN.

    ASSIGN
        lcJobMachineDataByItem     = iplcJobMachineData
        cRunMinutes                = STRING(ROUND(ipbf-job-mch.run-hr * 60, 0))
        cMRMinutes                 = STRING(ROUND(ipbf-job-mch.mr-hr * 60, 0))
        cMRComplete                = STRING(ipbf-job-mch.mr-complete)
        cRunComplete               = STRING(ipbf-job-mch.run-complete)
        .
    
    FOR EACH bf-mch-act NO-LOCK
        WHERE bf-mch-act.company  EQ ipbf-job-mch.company
          AND bf-mch-act.job      EQ ipbf-job-mch.job
          AND bf-mch-act.job-no   EQ ipbf-job-mch.job-no
          AND bf-mch-act.job-no2  EQ ipbf-job-mch.job-no2
          AND bf-mch-act.frm      EQ ipbf-job-mch.frm
          AND bf-mch-act.blank-no EQ ipbf-job-mch.blank-no
          AND bf-mch-act.m-code   EQ ipbf-job-mch.m-code:
        IF bf-mch-act.code EQ "MR" THEN
            ASSIGN
                lStartedMR        = TRUE
                iQuantityWastedMR = iQuantityWastedMR + bf-mch-act.waste
                dQuantityRun      = dQuantityRun + bf-mch-act.qty
                dHoursMR          = dHoursMR + bf-mch-act.hours
                .
        IF bf-mch-act.code EQ "Run" THEN
            ASSIGN
                lStartedRun        = TRUE
                iQuantityWastedRun = iQuantityWastedRun + bf-mch-act.waste
                dHoursRun          = dHoursRun + bf-mch-act.hours
                .
    END.

    ASSIGN
        lStarted        = lStartedMR OR lStartedRun
        iQuantityWasted = iQuantityWastedMR + iQuantityWastedRun
        .

   RUN pUpdatePOInfo(
        INPUT  ipbf-job-mch.company,
        INPUT  ipbf-job-mch.job,
        INPUT  ipbf-job-mch.job-no,
        INPUT  ipbf-job-mch.job-no2,
        INPUT  ipbf-job-mch.i-no,
        INPUT  ipbf-job-mch.frm,
        INPUT  ipbf-job-mch.blank-no,
        INPUT  "Machine",
        INPUT-OUTPUT lcJobMachineData
        ).
                 
    RUN pGetJobMaterialInfo (
        INPUT  ipbf-job-mch.company,
        INPUT  ipbf-job-mch.job,
        INPUT  ipbf-job-mch.job-no,
        INPUT  ipbf-job-mch.job-no2,
        INPUT  ipbf-job-mch.frm,
        INPUT  ipbf-job-mch.blank-no,
        INPUT  ipbf-job-mch.i-no,
        OUTPUT dBoardLength,   
        OUTPUT dBoardWidth,    
        OUTPUT dJobBoardIssued,
        OUTPUT cBoard,         
        OUTPUT cInk,           
        OUTPUT dInkQty,        
        OUTPUT cPallet,        
        OUTPUT dPalletMRP,        
        OUTPUT cMatType5,      
        OUTPUT dMatType5Qty,   
        OUTPUT cMatType6,      
        OUTPUT dMatType6Qty,   
        OUTPUT cVarnish,       
        OUTPUT cAdders,        
        OUTPUT dNoCases,       
        OUTPUT cCasesName,     
        OUTPUT cFilmName,      
        OUTPUT dRequiredQty,   
        OUTPUT dTotalMRP,      
        OUTPUT dMatLength,
        OUTPUT iNumUp
        ).
    
    IF dTotalMRP EQ ? THEN
        dTotalMRP = ipbf-job-mch.run-qty.
                             
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "StartedMR",STRING(lStartedMR)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "StartedRun",STRING(lStartedRun)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Started", STRING(lStarted)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "CompletedMR", cMRComplete).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "CompletedRun", cRunComplete).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Completed", STRING(lComplete)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "QuantityWastedMR", STRING(iQuantityWastedMR)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "QuantityWastedRun", STRING(iQuantityWastedRun)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "QuantityWasted", STRING(iQuantityWasted)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "QuantityRun", STRING(dQuantityRun)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "HoursMR", STRING(dHoursMR)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "HoursRun", STRING(dHoursRun)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RunMinutes",cRunMinutes).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MakeReadyMinutes",cMRMinutes).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "BoardLength", STRING(dBoardLength)).   
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "BoardWidth", STRING(dBoardWidth)).    
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "JobBoardIssued", STRING(dJobBoardIssued)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Board", cBoard).         
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "InkColor", cInk).      
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "InkQuantity", STRING(dInkQty)).        
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Pallet", cPallet).        
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "PalletMRP", STRING(dPalletMRP)).       
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MatType5Item", cMatType5).      
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MatType5Quantity", STRING(dMatType5Qty)).   
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MatType6Item", cMatType6).     
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MatType6Quantity", STRING(dMatType6Qty)).   
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Varnish", cVarnish).       
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "Adders", cAdders).       
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "NumberOfCases", STRING(dNoCases)).      
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "CasesName", cCasesName).    
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "FilmName", cFilmName).     
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "RequiredQuantity", STRING(dRequiredQty)).   
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "TotalMRP", STRING(dTotalMRP)).      
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "NumUp", STRING(iNumUp)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "LinearFeet", STRING(dTotalMRP * (dMatLength / 12))).
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "TotalPieces", STRING(dTotalMRP * iNumUp)).

    lcJobMachineDataByItem = oAttribute:ReplaceAttributes(lcJobMachineDataByItem, BUFFER ipbf-job-mch:HANDLE).

    RUN pUpdateEstimateBlankInfo(ipbf-job-mch.company, job.est-no, ipbf-job-mch.frm, ipbf-job-mch.blank-no, INPUT-OUTPUT lcJobMachineDataByItem).
    RUN pUpdateEstimateFormInfo(ipbf-job-mch.company, job.est-no, ipbf-job-mch.frm, INPUT-OUTPUT lcJobMachineDataByItem, INPUT-OUTPUT dMatLength).

    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MatLength", STRING(dMatLength)).
                
    FIND FIRST bf-mach NO-LOCK
         WHERE bf-mach.company EQ ipbf-job-mch.company
           AND bf-mach.m-code  EQ ipbf-job-mch.m-code
         NO-ERROR.
    IF AVAILABLE bf-mach THEN
        cMachineCodeSchedule = IF bf-mach.sch-m-code NE "" THEN bf-mach.sch-m-code ELSE ipbf-job-mch.m-code.
            
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineDataByItem, "MachineCodeSchedule",cMachineCodeSchedule).
    lcJobMachineDataByItem = oAttribute:ReplaceAttributes(lcJobMachineDataByItem, BUFFER bf-mach:HANDLE).
    
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
