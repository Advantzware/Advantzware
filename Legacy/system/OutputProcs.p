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
DEFINE VARIABLE gcContinue AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcNumError AS CHARACTER NO-UNDO.
ASSIGN 
    gcNumError = "#"
    gcContinue = CHR(187).
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION FormatForCSV RETURNS CHARACTER 
    (ipcValue AS CHARACTER) FORWARD.
    
FUNCTION FormatNumber RETURNS CHARACTER
    (ipdNumber AS DECIMAL,
    ipiLeftDigits AS INTEGER,
    ipiRightDigits AS INTEGER,
     iplComma AS LOGICAL) FORWARD.

FUNCTION FormatString RETURNS CHARACTER
    (ipcString AS CHARACTER,
    ipiCharacters AS INTEGER) FORWARD.

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */
PROCEDURE CloseOutput:
/*------------------------------------------------------------------------------
 Purpose:  Closes output
 Notes:
------------------------------------------------------------------------------*/
    OUTPUT STREAM sOutput CLOSE.

END PROCEDURE.

PROCEDURE GetBarDirFilePath:
/*------------------------------------------------------------------------------
 Purpose: Returns the entire path for the location of the data file
 Notes:  Wraps 
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDB AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilePath AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cBarDir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDB AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lUserSpecific AS LOGICAL NO-UNDO.
    
    RUN sys/ref/GetBarDir.p (ipcCompany, ipcDB, OUTPUT opcFilePath, OUTPUT cDB, OUTPUT lUserSpecific).
    
END PROCEDURE.

PROCEDURE InitializeOutputXprint:
/*------------------------------------------------------------------------------
 Purpose: Initialize XPrintOutput with default Font and FontSize
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPreview AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplModal AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFont AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFontSize AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAdditionalTags AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cInitTag AS CHARACTER NO-UNDO.

    RUN InitializeOutput(ipcOutputFile).
    
    IF ipcFont EQ "" THEN ipcFont = "Tahoma".
    IF ipiFontSize LT 8 THEN ipiFontSize = 8.
    cInitTag = "<F" + ipcFont + "><P" + TRIM(STRING(ipiFontSize,">9")) + ">".
    IF ipcAdditionalTags NE "" THEN cInitTag = ipcAdditionalTags + cInitTag.
    IF NOT iplModal THEN cInitTag = "<MODAL=NO>" + cInitTag. 
    IF iplPreview THEN cInitTag = "<PREVIEW>" + cInitTag.
     
    RUN WriteOutput(cInitTag, YES, YES).

END PROCEDURE.

PROCEDURE InitializeOutput:
/*------------------------------------------------------------------------------
 Purpose:  Initializes the stream given an output file 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
 
    OUTPUT STREAM sOutput TO  VALUE(ipcOutputFile).

END PROCEDURE.

PROCEDURE printFile EXTERNAL "xPrint.dll" :
    DEF INPUT PARAMETER A AS CHAR.

END.

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
        RUN custom/lmprint.p (ipcCompany, ipcQDFFile, cDB, cBarDir).
    ELSE
        RUN custom/lmprint.p (ipcCompany, ipcQDFFile, "", "").

END PROCEDURE.

PROCEDURE PrintXprintFile:
/*------------------------------------------------------------------------------
 Purpose: Wrapper for Printing XPrint File
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcXprintFile AS CHARACTER NO-UNDO.

    RUN printFile(ipcXPrintFile).

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

PROCEDURE WriteOutput:
/*------------------------------------------------------------------------------
 Purpose: Writes passed value to stream
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcText AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplUnformatted AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplSkip AS LOGICAL NO-UNDO.

    IF iplUnformatted THEN 
        PUT STREAM sOutput UNFORMATTED ipcText.
    ELSE 
        PUT STREAM sOutput ipcText.
    IF iplSkip THEN 
        PUT STREAM sOutput SKIP.
    
END PROCEDURE.

PROCEDURE WriteToXprint:
/*------------------------------------------------------------------------------
 Purpose: Wrapper on Write that prefixes Coordinates passed
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdR AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdC AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcText AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplBold AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUnderline AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRightJustified AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cCoordinates     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cText            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dUnderlineOffset AS DECIMAL   NO-UNDO INITIAL 0.25.

    IF iplUnderline THEN 
        ipdR = ipdR - dUnderlineOffset.
    cCoordinates = "<R" + TRIM(STRING(ipdR)) + ">".

    IF iplRightJustified THEN
        cCoordinates = cCoordinates + "<RIGHT=C".
    ELSE 
        cCoordinates = cCoordinates + "<C".
    cCoordinates = cCoordinates + TRIM(STRING(ipdC)) + ">".

    cText = cCoordinates + ipcText.

    IF iplBold THEN cText = "<B>" + cText + "</B>".
    IF iplUnderline THEN cText = "<U>" + cText + "</U>".
    RUN WriteOutput(cText,YES,NO).

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

FUNCTION FormatNumber RETURNS CHARACTER 
    ( ipdNumber AS DECIMAL , ipiLeftDigits AS INTEGER , ipiRightDigits AS INTEGER, iplComma AS LOGICAL):
/*------------------------------------------------------------------------------
 Purpose: Formats a number with left and right digits.  Handles problem when 
 size of number doesn't fit
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cErrorChar AS CHARACTER NO-UNDO.
 
    
    IF NOT iplComma OR ipiLeftDigits LE 3 THEN 
        cFormat = FILL(">",ipiLeftDigits - 1) + "9".
    ELSE DO:
        IF ipiLeftDigits GT 9 THEN cFormat = FILL(">",ipiLeftDigits - 9) + ",>>>,>>>,>>9".
        ELSE IF ipiLeftDigits GT 6 THEN cFormat = FILL(">",ipiLeftDigits - 6) + ",>>>,>>9".
        ELSE IF ipiLeftDigits GT 3 THEN cFormat = FILL(">",ipiLeftDigits - 6) + ",>>9". 
    END.
    IF ipiRightDigits GT 0 THEN 
        cFormat = cFormat + "." + Fill("9",ipiRightDigits).
    IF ipdNumber GE EXP(10, ipiLeftDigits) THEN  DO:
        cErrorChar = SUBSTRING(gcNumError, 1,1).
        cReturn = FILL(cErrorChar, LENGTH(cFormat)).
    END.
    ELSE
        cReturn = STRING(ipdNumber,cFormat).
            
    RETURN cReturn.
        
END FUNCTION.

FUNCTION FormatString RETURNS CHARACTER
    ( ipcString AS CHARACTER, ipiCharacters AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:  Formats string with number of characters.  If string is larger than what fits, 
 it auto adds a "cont" string to end
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLength AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLengthCont AS INTEGER NO-UNDO.
    
    ASSIGN 
        iLengthCont = LENGTH(TRIM(gcContinue))
        iLength = LENGTH(ipcString)
        .
    IF iLength GT ipiCharacters THEN 
        cReturn = SUBSTRING(ipcString,1,ipiCharacters - iLengthCont) + TRIM(gcContinue).
    ELSE 
        cReturn = ipcString.
    
    RETURN cReturn.
        
END FUNCTION.
