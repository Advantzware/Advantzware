
/*------------------------------------------------------------------------
    File        : SystemDataExport.p
    Purpose     : Exports data from a json file to the input table name

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Tue Nov 30 15:35:28 IST 2021
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER ipcTable         AS CHARACTER NO-UNDO. /* Database table name */
DEFINE INPUT  PARAMETER ipcFileName      AS CHARACTER NO-UNDO. /* File name to export */
DEFINE INPUT  PARAMETER ipcTableList     AS CHARACTER NO-UNDO. /* Additionally can pass all the tables that involved in the query. Developer has to build the query if a value is passed */
DEFINE INPUT  PARAMETER ipcWhereQuery    AS CHARACTER NO-UNDO. /* Optional where query. If not specified will export/import all data */
DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
/* ***************************  Definitions  ************************** */

DEFINE VARIABLE hdTTSystemData AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTableBuffer  AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTTBuffer     AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTableBuffers AS HANDLE NO-UNDO EXTENT.
DEFINE VARIABLE iIndex         AS INTEGER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST ASI._file NO-LOCK
     WHERE ASI._file._file-name EQ ipcTable
     NO-ERROR.
FIND FIRST Audit._file NO-LOCK
     WHERE Audit._file._file-name EQ ipcTable
     NO-ERROR.
IF NOT AVAILABLE ASI._file AND NOT AVAILABLE Audit._file THEN DO:
    opcMessage = "Invalid DB table " + ipcTable + " passed as input parameter".
    RETURN.
END.

IF ipcTableList NE "" THEN DO:
    EXTENT(hdTableBuffers) = NUM-ENTRIES(ipcTableList).
    DO iIndex = 1 TO NUM-ENTRIES(ipcTableList):   
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name EQ ENTRY(iIndex, ipcTableList)
             NO-ERROR.
        FIND FIRST Audit._file NO-LOCK
             WHERE Audit._file._file-name EQ ENTRY(iIndex, ipcTableList)
             NO-ERROR.
        IF NOT AVAILABLE ASI._file AND NOT AVAILABLE Audit._file THEN DO:
            opcMessage = "Invalid DB table " + ipcTable + " passed as input parameter".
            RETURN.
        END.
         
        CREATE BUFFER hdTableBuffers[iIndex] FOR TABLE ENTRY(iIndex, ipcTableList).
        IF ENTRY(iIndex, ipcTableList) EQ ipcTable THEN
            hdTableBuffer = hdTableBuffers[iIndex].
    END.
END.
ELSE
    CREATE BUFFER hdTableBuffer FOR TABLE ipcTable.

CREATE TEMP-TABLE hdTTSystemData.
hdTTSystemData:CREATE-LIKE (hdTableBuffer).
hdTTSystemData:TEMP-TABLE-PREPARE(ipcTable).

hdTTBuffer = hdTTSystemData:DEFAULT-BUFFER-HANDLE.

RUN pExport.

FINALLY:
    IF VALID-HANDLE (hdTableBuffer) THEN
        DELETE OBJECT hdTableBuffer.
    
    IF VALID-HANDLE (hdTTSystemData) THEN
        DELETE OBJECT hdTTSystemData.
    
    DO iIndex = 1 TO EXTENT (hdTableBuffers):
        IF VALID-HANDLE(hdTableBuffers[iIndex]) THEN
            DELETE OBJECT hdTableBuffers[iIndex].
    END.
END FINALLY.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pExport:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdQuery AS HANDLE NO-UNDO.

    CREATE QUERY hdQuery.
    IF ipcTableList NE "" THEN DO:
        DO iIndex = 1 TO EXTENT(hdTableBuffers):
            hdQuery:ADD-BUFFER (hdTableBuffers[iIndex]).
        END.
        hdQuery:QUERY-PREPARE (ipcWhereQuery).        
    END.    
    ELSE DO:
        hdQuery:ADD-BUFFER (hdTableBuffer).
        hdQuery:QUERY-PREPARE ("FOR EACH " + hdTableBuffer:NAME + " NO-LOCK " + (IF ipcWhereQuery NE "" THEN " WHERE " + ipcWhereQuery ELSE "")).
    END.    

    hdQuery:QUERY-OPEN ().
    
    hdQuery:GET-NEXT().
    
    DO WHILE NOT hdQuery:QUERY-OFF-END:
        hdTTBuffer:BUFFER-CREATE().
        hdTTBuffer:BUFFER-COPY(hdTableBuffer). 
    
        hdQuery:GET-NEXT().    
    END.
    
    hdQuery:QUERY-CLOSE ().
    
    hdTTSystemData:WRITE-JSON ("FILE", IF ipcFileName NE "" THEN ipcFileName ELSE ipcTable + ".json", TRUE /* Formatted */, ?, ?, TRUE /* Omit Outer object */ ).

    FILE-INFO:FILE-NAME = IF ipcFileName NE "" THEN ipcFileName ELSE ipcTable + ".json".
    
    opcMessage = "File exported to " + FILE-INFO:FULL-PATHNAME.  
    
    IF VALID-HANDLE (hdQuery) THEN
        DELETE OBJECT hdQuery.    
END PROCEDURE.
