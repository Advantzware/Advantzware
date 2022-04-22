
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
DEFINE VARIABLE lcJobsData             AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobsData       AS LONGCHAR  NO-UNDO.

/* Variables to store job material request data */
DEFINE VARIABLE lcJobHeaderData        AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobHeaderData  AS LONGCHAR  NO-UNDO.

/* Variables to store job material request data */
DEFINE VARIABLE lcJobMatData           AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobMatData     AS LONGCHAR  NO-UNDO.
    
/* Variables to store job machine request data */
DEFINE VARIABLE lcJobMachineData       AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobMachineData AS LONGCHAR  NO-UNDO.
    
/* Variables to store job Preperation request data */
DEFINE VARIABLE lcJobPrepData          AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatJobPrepData    AS LONGCHAR  NO-UNDO.
    
/*Job Material variables*/
DEFINE VARIABLE cCrossGrain            AS CHARACTER NO-UNDO.
    
/*Job Machine variables*/
    
DEFINE VARIABLE cRunMinutes            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMRMinutes             AS CHARACTER NO-UNDO.
    
/* Job Preps Variables */
DEFINE VARIABLE cSIMON                 AS CHARACTER NO-UNDO.

DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.
    
DEFINE BUFFER bf-APIOutboundDetail FOR APIOutboundDetail.
DEFINE BUFFER bf-job-mat           FOR job-mat.
DEFINE BUFFER bf-itemfg            FOR itemfg.      

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
/**********************  Preprocessor Definitions  ******************** */




/* ************************  Function Prototypes ********************** */

/* ***************************  Main Block  *************************** */
{api/SendJob.i}

/* **********************  Internal Procedures  *********************** */


RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
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
           AND APIOutboundDetail.parentID      EQ "SendJob"
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
    
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ "JobHeader"
           AND bf-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
         NO-ERROR.
         
    IF AVAILABLE bf-APIOutboundDetail THEN 
    DO:       
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
              AND job-hdr.job-no  EQ job.job-no
              AND job-hdr.job-no2 EQ job.job-no2: 
                       
            FIND FIRST oe-ord NO-LOCK 
                 WHERE oe-ord.company EQ job-hdr.company
                   AND oe-ord.ord-no  EQ job-hdr.ord-no 
                 NO-ERROR.
                
            FIND FIRST bf-itemfg NO-LOCK
                 WHERE bf-itemfg.company EQ job-hdr.company 
                   AND bf-itemfg.i-no    EQ job-hdr.i-no
                 NO-ERROR.
                         
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ job-hdr.company
                   AND oe-ordl.ord-no  EQ job-hdr.ord-no
                   AND oe-ordl.job-no  EQ job-hdr.job-no
                   AND oe-ordl.job-no2 EQ job-hdr.job-no2
                   AND oe-ordl.i-no    EQ job-hdr.i-no
                 NO-ERROR.  
                                            
            lcJobHeaderData = bf-APIOutboundDetail.data.
                
            lcJobHeaderData = oAttribute:ReplaceAttributes(lcJobHeaderData, BUFFER job-hdr:HANDLE).    
            lcJobHeaderData = oAttribute:ReplaceAttributes(lcJobHeaderData, BUFFER oe-ord:HANDLE).
            lcJobHeaderData = oAttribute:ReplaceAttributes(lcJobHeaderData, BUFFER bf-itemfg:HANDLE).
            lcJobHeaderData = oAttribute:ReplaceAttributes(lcJobHeaderData, BUFFER oe-ordl:HANDLE).
            
            RUN pUpdateCustomerInfo(job-hdr.company, job-hdr.cust-no, INPUT-OUTPUT lcJobHeaderData).
             
            lcConcatJobHeaderData = lcConcatJobHeaderData + lcJobHeaderData.
        END.
    END. 
        
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ "JobMaterial"
           AND bf-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
         NO-ERROR. 
    IF AVAILABLE bf-APIOutboundDetail THEN 
    DO:  
        FOR EACH job-mat NO-LOCK
            WHERE job-mat.company EQ job.company
              AND job-mat.job-no  EQ job.job-no
              AND job-mat.job-no2 EQ job.job-no2:
                
            lcJobMatData = bf-APIOutboundDetail.data.
                        
            FOR EACH job-hdr NO-LOCK
                WHERE job-hdr.company EQ job-mat.company
                  AND job-hdr.job-no  EQ job-mat.job-no
                  AND job-hdr.job-no2 EQ job-mat.job-no2
                  AND job-hdr.frm     EQ job-mat.frm,
                EACH ef NO-LOCK
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

            cCrossGrain = IF job-mat.xGrain EQ "N" THEN "NO" 
                          ELSE IF job-mat.xGrain EQ "S" THEN "(S)heet"
                          ELSE IF job-mat.xGrain EQ "B" THEN "(B)lank" 
                          ELSE job-mat.xgrain.

            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMatData, "CrossGrain", cCrossGrain).

            lcJobMatData = oAttribute:ReplaceAttributes(lcJobMatData, BUFFER job-mat:HANDLE).

            RUN pUpdateRMItemInfo (job-mat.company, job-mat.i-no, INPUT-OUTPUT lcJobMatData).
                             
            lcConcatJobMatData  = lcConcatJobMatData  + "~n" + lcJobMatData.                                              
        END.
    END.
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ "JobMachine"
           AND bf-APIOutboundDetail.parentID      EQ APIOutboundDetail.detailID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN 
    DO:    
        FOR EACH job-mch NO-LOCK
            WHERE job-mch.company EQ job.company
              AND job-mch.job-no  EQ job.job-no
              AND job-mch.job-no2 EQ job.job-no2:
            
            RUN pUpdatePOInfo(
                INPUT  job-mch.company,
                INPUT  job-mch.job,
                INPUT  job-mch.job-no,
                INPUT  job-mch.job-no2,
                INPUT  job-mch.i-no,
                INPUT  job-mch.frm,
                INPUT  job-mch.blank-no,
                INPUT  "Machine",
                INPUT-OUTPUT lcJobMachineData
                ).
                         
            RUN pGetJobMaterialInfo (
                INPUT  job-mch.company,
                INPUT  job-mch.job,
                INPUT  job-mch.job-no,
                INPUT  job-mch.job-no2,
                INPUT  job-mch.frm,
                INPUT  job-mch.blank-no,
                INPUT  job-mch.i-no,
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
            
            ASSIGN              
                lcJobMachineData = bf-APIOutboundDetail.data
                cRunMinutes      = STRING(ROUND(job-mch.run-hr * 60, 0))
                cMRMinutes       = STRING(ROUND(job-mch.mr-hr * 60, 0))
                . 
            
            IF dTotalMRP EQ ? THEN
                dTotalMRP = job-mch.run-qty.
            
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "RunMinutes", cRunMinutes).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "BoardLength", STRING(dBoardLength)).   
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "BoardWidth", STRING(dBoardWidth)).    
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "JobBoardIssued", STRING(dJobBoardIssued)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "Board", cBoard).         
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "InkColor", cInk).      
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "InkQuantity", STRING(dInkQty)).        
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "Pallet", cPallet).        
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "PalletMRP", STRING(dPalletMRP)).       
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "MatType5Item", cMatType5).      
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "MatType5Quantity", STRING(dMatType5Qty)).   
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "MatType6Item", cMatType6).     
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "MatType6Quantity", STRING(dMatType6Qty)).   
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "Varnish", cVarnish).       
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "Adders", cAdders).       
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "NumberOfCases", STRING(dNoCases)).      
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "CasesName", cCasesName).    
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "FilmName", cFilmName).     
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "RequiredQuantity", STRING(dRequiredQty)).   
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "TotalMRP", STRING(dTotalMRP)).      
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "NumUp", STRING(iNumUp)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "LinearFeet", STRING(dTotalMRP * (dMatLength / 12))).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "TotalPieces", STRING(dTotalMRP * iNumUp)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "RunMinutes", cRunMinutes).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "MakeReadyMinutes", cMRMinutes).
            lcJobMachineData = oAttribute:ReplaceAttributes(lcJobMachineData, BUFFER job-mch:HANDLE).
            
            RUN pUpdateEstimateBlankInfo(job-mch.company, job.est-no, job-mch.frm, job-mch.blank-no, INPUT-OUTPUT lcJobMachineData).
            RUN pUpdateEstimateFormInfo(job-mch.company, job.est-no, job-mch.frm, INPUT-OUTPUT lcJobMachineData, INPUT-OUTPUT dMatLength).

            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobMachineData, "MatLength", STRING(dMatLength)).
            
            lcConcatJobMachineData = lcConcatJobMachineData +  "~n" + lcJobMachineData.         
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

    lcJobsData       = REPLACE(lcJobsData, "$JobHeader$", lcConcatJobHeaderData).
    lcJobsData       = REPLACE(lcJobsData, "$JobMaterial$", lcConcatJobMatData).
    lcJobsData       = REPLACE(lcJobsData, "$JobMachine$", lcConcatJobMachineData).
    lcJobsData       = REPLACE(lcJobsData, "$JobPrep$", lcConcatJobPrepData).
        
    oAttribute:UpdateRequestData(INPUT-OUTPUT lcJobsData, "IsPriority", STRING(job.priority EQ 1,"true/false")).
    lcJobsData = oAttribute:ReplaceAttributes(lcJobsData, BUFFER job:HANDLE).
        
    ioplcRequestData = REPLACE(ioplcRequestData, "$Jobs$", lcJobsData).   
END.                        
    
ASSIGN
    oplSuccess = TRUE
    opcMessage = "Success"
    .

FINALLY:
    IF VALID-OBJECT (oAttribute) THEN
        DELETE OBJECT oAttribute.

END FINALLY.        

