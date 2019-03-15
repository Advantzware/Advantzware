
/*------------------------------------------------------------------------
    File        : OutputProcs.p
    Purpose     : 

    Syntax      :

    Description : Various procedures for output of data

    Author(s)   : BV
    Created     : Sun Mar 03 19:39:10 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE STREAM sOutput.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION FormatForCSV RETURNS CHARACTER 
    (ipcValue AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE PrintLabelMatrixFile:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Label Matrix File
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcQDFFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDB AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cBarDir       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDB           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lUserSpecific AS LOGICAL   NO-UNDO.


    RUN sys/ref/GetBarDir.p (ipcCompany, ipcDB, OUTPUT cBarDir, OUTPUT cDB, OUTPUT lUserSpecific).

    IF lUserSpecific THEN 
        RUN custom/lmprint.p (ipcQDFFile, cDB, cBarDir).
    ELSE
        RUN custom/lmprint.p (ipcQDFFile, "", "").

END PROCEDURE.

PROCEDURE TempTableToCSV:
    /*------------------------------------------------------------------------------ 
         Purpose: Exports the contents of any temp-table into CSV    
         Notes: 
        ------------------------------------------------------------------------------*/ 
    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO. 
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER iplHeader AS LOGICAL NO-UNDO.
  
    DEFINE VARIABLE hQuery  AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE iIndex  AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cTTName AS CHARACTER NO-UNDO. 
    
    
    cTTName = iphTT:NAME. 
    IF iplHeader THEN 
    DO:
        OUTPUT STREAM sOutput to VALUE(ipcFileName). 
        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
            PUT STREAM sOutput UNFORMATTED iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):COLUMN-LABEL + ",". 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END.
    ELSE 
        OUTPUT STREAM sOutput to VALUE(ipcFileName) APPEND. 
    CREATE QUERY hQuery. 
    hQuery:SET-BUFFERS (iphTT:DEFAULT-BUFFER-HANDLE). 
    hQuery:QUERY-PREPARE("FOR EACH " + cTTName). 
    hQuery:QUERY-OPEN().
    REPEAT:   
        hQuery:GET-NEXT().   
        IF hQuery:QUERY-OFF-END THEN LEAVE.   
        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
            PUT STREAM sOutput UNFORMATTED  
                '"' FormatForCSV(iphTT:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):BUFFER-VALUE) '",'. 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END. 
    OUTPUT STREAM sOutput CLOSE.


END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION FormatForCSV RETURNS CHARACTER 
    ( ipcValue AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Fixes the input character value and returns a CSV friendly text
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cInvalidChars AS CHARACTER NO-UNDO INITIAL "~",#".
    DEFINE VARIABLE cReplaceChars AS CHARACTER NO-UNDO INITIAL "'',". 
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
    
 
    DO iCount = 1 TO NUM-ENTRIES(cInvalidChars):
        ipcValue = REPLACE(ipcValue,ENTRY(iCount,cInvalidChars),ENTRY(iCount,cReplaceChars)).
    END.
    RETURN ipcValue.   

		
END FUNCTION.

