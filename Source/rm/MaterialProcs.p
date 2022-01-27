
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
DEFINE VARIABLE gcMaterialCalculationTypeList AS CHARACTER NO-UNDO INITIAL "ByFGWeight,ByDefault,ByBlankTotalSize,ByBlankNetSize,ByFormTotalSize,ByFormNetSize,ByFormLength".

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE Material_GetCalculationTypeList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcCalculationTypeList AS CHARACTER NO-UNDO.

    opcCalculationTypeList = gcMaterialCalculationTypeList.
END PROCEDURE.

