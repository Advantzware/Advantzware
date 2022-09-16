
/*------------------------------------------------------------------------
    File        : ttEstMisc.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Aug 04 01:50:31 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttEstMisc NO-UNDO 
    LIKE estMisc
    FIELD isFlatFee   AS LOGICAL
    FIELD sourceType  AS CHARACTER
    FIELD sourceRowID AS ROWID
    .
