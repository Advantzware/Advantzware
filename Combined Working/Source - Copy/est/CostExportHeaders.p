
/*------------------------------------------------------------------------
    File        : CostExportHeaders.p
    Purpose     : 

    Syntax      :

    Description : Exports the contents of Headers to a file

    Author(s)   : BV
    Created     : Wed Feb 07 23:38:21 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFilePrefix AS CHARACTER NO-UNDO.

{est/CostTempTables.i "shared"}
DEFINE STREAM csvOutput.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExportType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSaveToDB AS LOGICAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetFullFilePath RETURNS CHARACTER 
	(ipcFilePrefix AS CHARACTER, ipcFileFolder AS CHARACTER, ipcType AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */

RUN pGetExportConfig(ipcCompany, OUTPUT cFile, OUTPUT cExportType, OUTPUT lSaveToDB).

cFile = fGetFullFilePath(ipcFilePrefix, cFile, cExportType).

CASE cExportType:
    WHEN "XML" THEN RUN pExportXML(cFile).
    WHEN "JSON" THEN RUN pExportJSON(cFile).
    WHEN "CSV" THEN RUN pExportCSV(cFile).
END.

IF lSaveToDB THEN RUN pSaveToDB.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetExportConfig:
/*------------------------------------------------------------------------------
 Purpose: Tests value of NK1 for output/save options 
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcFilePath AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcExportType AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER oplSaveToDB AS LOGICAL NO-UNDO.

DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE cResult AS CHARACTER NO-UNDO. 

RUN sys/ref/nk1Look.p(INPUT ipcCompany,
                      INPUT "CECostSave",
                      INPUT "C",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT cResult,
                      OUTPUT lFound).

IF NOT lFound OR cResult EQ "" 
    THEN opcFilePath = "C:\tmp".
    ELSE opcFilePath = cResult.

cResult = "".
RUN sys/ref/nk1Look.p(INPUT ipcCompany,
                      INPUT "CECostSave",
                      INPUT "L",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT cResult,
                      OUTPUT lFound).
oplSaveToDB = lFound AND cResult EQ "YES". 

cResult = "".
RUN sys/ref/nk1Look.p(INPUT ipcCompany,
                      INPUT "CECostSave",
                      INPUT "I",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT cResult,
                      OUTPUT lFound).
CASE cResult:
    WHEN "1" THEN opcExportType = "CSV".
    WHEN "2" THEN opcExportType = "XML".
    WHEN "3" THEN opcExportType = "JSON". 
END CASE.

END PROCEDURE.

PROCEDURE pExportCSV:
/*------------------------------------------------------------------------------
 Purpose: Dumps the contents of the table to CSV
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.

DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

OUTPUT STREAM csvOutput TO VALUE(ipcFile).
DO iIndex = 1 TO TEMP-TABLE ttCostHeader:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
    PUT STREAM csvOutput UNFORMATTED TEMP-TABLE ttCostHeader:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):COLUMN-LABEL ",".
END.
PUT STREAM csvOutput SKIP.

FOR EACH ttCostHeader NO-LOCK: 

  EXPORT STREAM csvOutput DELIMITER "," 
    ttCostHeader 
    . 
END. 

OUTPUT STREAM csvOutput CLOSE. 


END PROCEDURE.

PROCEDURE pExportJSON:
/*------------------------------------------------------------------------------
 Purpose: Exports the contents of the temp-table to XML
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.

DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRetOK      AS LOGICAL   NO-UNDO.


/* Code to populate the temp-table */  
ASSIGN  
  cTargetType = "file" 
  lFormatted  = TRUE. 
lRetOK = TEMP-TABLE ttCostHeader:WRITE-JSON(cTargetType, ipcFile, lFormatted).

END PROCEDURE.

PROCEDURE pExportXML:
/*------------------------------------------------------------------------------
 Purpose: Exports the contents of the temp-table to XML
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.

DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRetOK          AS LOGICAL   NO-UNDO.

/* Code to populate the temp-table  */
ASSIGN
  cTargetType     = "file"
  lFormatted      = TRUE
  cEncoding       = ?
  cSchemaLocation = ?
  lWriteSchema    = FALSE
  lMinSchema      = FALSE.

lRetOK = TEMP-TABLE ttCostHeader:WRITE-XML(cTargetType, ipcFile,lFormatted, cEncoding,
  cSchemaLocation, lWriteSchema, lMinSchema).

END PROCEDURE.

PROCEDURE pSaveToDB:
/*------------------------------------------------------------------------------
 Purpose: Write the contents of the temp-table to the DB
 Notes:
------------------------------------------------------------------------------*/
FOR EACH ttCostHeader:
    CREATE costHeader.
    BUFFER-COPY ttCostHeader TO costHeader.
END.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetFullFilePath RETURNS CHARACTER 
	(ipcFilePrefix AS CHARACTER, ipcFileFolder AS CHARACTER, ipcType AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns the full file path with unique file name
 Notes:
------------------------------------------------------------------------------*/	
DEFINE VARIABLE cFullPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExtension AS CHARACTER NO-UNDO.

ASSIGN 
    ipcFileFolder = TRIM(ipcFileFolder,"\")
    ipcFileFolder = TRIM(ipcFileFolder,"/")
    .

CASE ipcType:
    WHEN "XML" THEN cExtension = ".xml". 
    WHEN "JSON" THEN cExtension = ".json".
    WHEN "CSV" THEN cExtension = ".csv".
END CASE.

cFullPath = ipcFileFolder + "\" + ipcFilePrefix + STRING(TODAY,"99999999") + STRING(TIME) + cExtension.

RETURN cFullPath.		

END FUNCTION.

