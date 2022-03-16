
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
DEFINE VARIABLE gcCharCategory AS CHARACTER NO-UNDO INIT "C".
DEFINE VARIABLE gcCharType AS CHARACTER NO-UNDO INIT "T". 
DEFINE VARIABLE gcCharLength AS CHARACTER NO-UNDO INIT "L".
DEFINE VARIABLE gcCharWidth AS CHARACTER NO-UNDO INIT "W".
DEFINE VARIABLE gcCharHash AS CHARACTER NO-UNDO INIT "#".
DEFINE VARIABLE gcCharHyphen AS CHARACTER NO-UNDO INIT "-".
DEFINE VARIABLE gcCharSeq AS CHARACTER NO-UNDO INIT "9".
DEFINE VARIABLE gcCharZero AS CHARACTER NO-UNDO INIT "0".

/* ********************  Preprocessor Definitions  ******************** */


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

PROCEDURE Material_GetNextItemId:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType         AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER ipcLength       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWidth        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCategory     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemId      AS CHARACTER NO-UNDO.
    
    RUN pGetNextItemId(INPUT ipcCompany, INPUT ipcType, INPUT ipcLength, INPUT ipcWidth, INPUT ipcCategory, OUTPUT opcItemId).
    
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

PROCEDURE pGetNextItemId PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType         AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER ipcLength       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWidth        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCategory     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemId      AS CHARACTER NO-UNDO.     
        
    DEFINE VARIABLE cItemFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBeginItemID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemSeq AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE iCharPos AS INTEGER NO-UNDO.
    DEFINE VARIABLE cChar AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNextSeq AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCategory AS INTEGER NO-UNDO.
    DEFINE VARIABLE iType AS INTEGER NO-UNDO.    
    DEFINE VARIABLE iLength AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWidth AS INTEGER NO-UNDO.
    
    RUN spGetSettingByName("RawMaterialItemMask", OUTPUT cItemFormat). 
    
    ipcLength = REPLACE(ipcLength,".","").
    ipcWidth = REPLACE(ipcWidth,".","").
              
    IF cItemFormat EQ ? THEN RETURN.          
    DO iCharPos = 1 TO LENGTH(cItemFormat):
    cChar = SUBSTRING(cItemFormat,iCharPos,1).   
               
        CASE cChar:
            WHEN gcCharCategory THEN 
                ASSIGN 
                    iCategory = iCategory + 1 
                    cBeginItemID = cBeginItemID + SUBSTRING(ipcCategory,iCategory,1) 
                    .
            WHEN gcCharType THEN 
                ASSIGN  
                    iType = iType + 1
                    cBeginItemID = cBeginItemID + SUBSTRING(ipcType,iType,1) 
                    .             
            WHEN gcCharLength THEN 
                ASSIGN  
                    iLength = iLength + 1
                    cBeginItemID = cBeginItemID + SUBSTRING(ipcLength,iLength,1) 
                    .
            WHEN gcCharWidth THEN 
                ASSIGN 
                    iWidth = iWidth + 1
                    cBeginItemID = cBeginItemID + SUBSTRING(ipcWidth,iWidth,1) 
                    .        
            WHEN gcCharHash THEN 
                ASSIGN  
                    cItemSeq  = cItemSeq + "9"
                    .
            WHEN gcCharHyphen THEN 
                ASSIGN  
                    cBeginItemID = cBeginItemID + gcCharHyphen 
                    .
            WHEN gcCharSeq THEN 
                ASSIGN 
                    cItemSeq  = cItemSeq + "9"
                    .
            WHEN gcCharZero THEN 
                cBeginItemID = cBeginItemID + gcCharZero 
                    .            
            OTHERWISE 
                cBeginItemID = cBeginItemID + cChar
                .
          END CASE .      
    END.
    
    RUN pGetNextSequence(INPUT ipcCompany, INPUT cBeginItemID, INPUT cItemSeq, OUTPUT cNextSeq).
    opcItemId = cBeginItemID + STRING(integer(cNextSeq),cItemSeq) .     
    
END PROCEDURE.

PROCEDURE pGetNextSequence PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcBeginItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcItemSeq     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ipiNextSeq     AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE iNextSequence AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-item FOR ITEM.
    
    FOR EACH bf-item NO-LOCK 
        WHERE bf-item.company EQ ipcCompany
        AND bf-item.i-no BEGINS ipcBeginItemID
        BY bf-item.i-no DESCENDING:
        iNextSequence = INT(SUBSTRING(bf-item.i-no,LENGTH(ipcBeginItemID) + 1, LENGTH(ipcItemSeq))) NO-ERROR. 
        IF ERROR-STATUS:ERROR THEN 
            iNextSequence = 0.
        ELSE 
            LEAVE.
    END.
    ipiNextSeq = iNextSequence + 1.
END PROCEDURE.

