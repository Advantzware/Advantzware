
/*------------------------------------------------------------------------
    File        : MaterialProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Mon Mar 01 13:55:41 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE gcMaterialCalculationTypeList AS CHARACTER NO-UNDO INITIAL "ByDefault,ByFGWeight,BlankTotalSize,BlankNetSize,FormTotalSize,FormNetSize,FormLength".
DEFINE VARIABLE gcMaterialCalculationTypeLabelList AS CHARACTER NO-UNDO INITIAL "Default,FGWeight,BlankTotalSize,BlankNetSize,FormTotalSize,FormNetSize,FormLength".
DEFINE VARIABLE gcSystemTypeList AS CHARACTER NO-UNDO INITIAL "Board,Glue,Ink/Coating,Packing,Foam,Wood,Wax,Window,Misc,Adders,Die,Plates".

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fIsMatlGroup RETURNS LOGICAL 
	(INPUT ipcCompany AS CHAR,
	 INPUT ipcItemNo AS CHAR,
	 INPUT ipcMaterialTypeGroup AS CHAR) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE Material_GetCalculationTypeList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcCalculationTypeList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE cInitVal AS CHARACTER NO-UNDO.
    
    ASSIGN cInitVal = ","
    opcCalculationTypeList = cInitVal.
    DO iCount = 1 TO NUM-ENTRIES(gcMaterialCalculationTypeList):
      opcCalculationTypeList = opcCalculationTypeList + ENTRY(iCount,gcMaterialCalculationTypeLabelList) + "," 
                                                      +  ENTRY(iCount,gcMaterialCalculationTypeList) + ",".
    END.
    IF opcCalculationTypeList NE cInitVal THEN 
      opcCalculationTypeList = TRIM(opcCalculationTypeList,",").

   
END PROCEDURE.

PROCEDURE Material_GetMaterialTypeListFromGroup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcMaterialTypeGroup AS CHAR.
    DEF OUTPUT PARAMETER opcMaterialTypeList AS CHAR.
    DEF VAR cMaterialTypeList AS CHAR.

    FOR EACH materialType NO-LOCK WHERE 
        materialTypeGroup EQ ipcMaterialTypeGroup:
        ASSIGN 
            cMaterialTypeList = cMaterialTypeList + materialType.materialType + ",".
    END.
    ASSIGN 
        opcMaterialTypeList = TRIM(cMaterialTypeList,",").

END PROCEDURE.

PROCEDURE Material_GetSystemTypeList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcSystemTypeList AS CHARACTER NO-UNDO.

    opcSystemTypeList = gcSystemTypeList.
END PROCEDURE.

PROCEDURE Material_UpdateMaterialSystemType:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMaterialType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMaterialTypeGroup AS CHARACTER NO-UNDO. 
    DEFINE BUFFER bf-item FOR ITEM.
    
     FOR EACH bf-item EXCLUSIVE-LOCK
        WHERE bf-item.company EQ ipcCompany
          AND bf-item.mat-type EQ ipcMaterialType :

            bf-item.materialType = ipcMaterialTypeGroup.             
     END.       
END PROCEDURE.

PROCEDURE Material_UpdateJobMaterialAutoIssue:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMaterialType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplAutoIssue    AS LOGICAL NO-UNDO.
        
    RUN pUpdateJobMaterialAutoIssue (
                           ipcCompany,
                           ipcMaterialType, 
                           iplAutoIssue
                           ).            
END PROCEDURE.

PROCEDURE pUpdateJobMaterialAutoIssue PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMaterialType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplAutoIssue    AS LOGICAL NO-UNDO.
        
    DEFINE BUFFER bf-item FOR ITEM.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    
    FOR EACH job NO-LOCK
        WHERE job.company EQ ipcCompany
        AND job.opened EQ YES, 
        EACH bf-job-mat EXCLUSIVE-LOCK 
        WHERE bf-job-mat.company EQ ipcCompany
        AND bf-job-mat.job EQ job.job
        AND bf-job-mat.job-no EQ job.job-no
        AND bf-job-mat.job-no2 EQ job.job-no2 ,     
        FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipcCompany
        AND bf-item.i-no EQ bf-job-mat.i-no
        AND bf-item.mat-type EQ ipcMaterialType :

            bf-job-mat.post = iplAutoIssue.             
     END. 
     RELEASE bf-job-mat.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fIsMatlGroup RETURNS LOGICAL 
	( INPUT ipcCompany AS CHAR, INPUT ipcItemNo AS CHAR, INPUT ipcMaterialTypeGroup AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
    
    ASSIGN 
        lResult = FALSE.
        
    FIND FIRST ITEM NO-LOCK WHERE 
        ITEM.company EQ ipcCompany AND 
        ITEM.i-no EQ ipcItemNo
        NO-ERROR.
        
    IF AVAIL ITEM THEN DO:
        FIND FIRST materialType NO-LOCK where
            materialType.company EQ ITEM.company AND 
            materialType.materialType EQ ITEM.mat-type 
            NO-ERROR.
        IF AVAIL materialType 
        AND materialType.materialTypeGroup EQ ipcMaterialTypeGroup THEN ASSIGN 
            lResult = TRUE.
        ELSE ASSIGN lResult = FALSE.
    END.

    RETURN lResult.
		
END FUNCTION.

