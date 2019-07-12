
/*------------------------------------------------------------------------
    File        : JSONProcs.p
    Purpose     : 

    Syntax      :

    Description : JSON Handler

    Author(s)   : Porandla Mithun
    Created     : Mon Jun 24 07:57:30 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */
     
/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */
PROCEDURE WriteJSONToTempTable:
    DEFINE INPUT  PARAMETER iplcData      AS LONGCHAR  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSourceType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcReadMode   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ophdTempTable AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

    CREATE TEMP-TABLE ophdTempTable.

    oplSuccess = ophdTempTable:READ-JSON(ipcSourceType, iplcData, ipcReadMode) NO-ERROR.
    IF NOT oplSuccess THEN
        opcMessage  = "bad JSON".
END PROCEDURE.

PROCEDURE WriteTempTableToJSON:
    DEFINE INPUT  PARAMETER iphdTempTable AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTargetType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplFormatted  AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEncoding   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcData      AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.
           
    oplSuccess = iphdTempTable:WRITE-JSON(ipcTargetType, oplcData, iplFormatted, ipcEncoding, FALSE, TRUE) NO-ERROR.    
    IF NOT oplSuccess THEN
        opcMessage = "bad JSON".
END PROCEDURE.
