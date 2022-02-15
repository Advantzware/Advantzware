
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

PROCEDURE Material_GetSystemTypeList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcSystemTypeList AS CHARACTER NO-UNDO.

    opcSystemTypeList = gcSystemTypeList.
END PROCEDURE.

