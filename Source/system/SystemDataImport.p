
/*------------------------------------------------------------------------
    File        : SystemDataImport.p
    Purpose     : Imports data from a json file to the input table name

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Tue Nov 30 15:35:28 IST 2021
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER ipcTable         AS CHARACTER NO-UNDO. /* Database table name */
DEFINE INPUT  PARAMETER ipcFileName      AS CHARACTER NO-UNDO. /* File name to import */
DEFINE INPUT  PARAMETER ipcWhereQuery    AS CHARACTER NO-UNDO. /* Optional where query for records to be loaded */
DEFINE INPUT  PARAMETER iplDeleteRecords AS LOGICAL   NO-UNDO. /* Deletes database records before importing */
DEFINE INPUT  PARAMETER ipcDeleteQuery   AS CHARACTER NO-UNDO. /* Optional where query for the records that needs to be deleted. If empty then deletes all records  */
DEFINE INPUT  PARAMETER ipcKeyFields     AS CHARACTER NO-UNDO. /* If iplDeleteRecords is false, then uses the key fields to compare and replace data */
DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
/* ***************************  Definitions  ************************** */

DEFINE VARIABLE hdTTSystemData AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdTableBuffer  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdTTBuffer     AS HANDLE  NO-UNDO.
DEFINE VARIABLE iFieldCount    AS INTEGER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND FIRST ASI._file NO-LOCK
     WHERE ASI._file._file-name = ipcTable
     NO-ERROR.
IF NOT AVAILABLE ASI._file THEN DO:
    opcMessage = "Invalid DB table " + ipcTable + " passed as input parameter".
    RETURN.
END.

IF SEARCH(IF ipcFileName NE "" THEN ipcFileName ELSE ipcTable + ".json") EQ ? THEN DO:
    opcMessage = "File '" + (IF ipcFileName NE "" THEN ipcFileName ELSE ipcTable + ".json") + "' does not exist".
    RETURN.
END.

IF NOT iplDeleteRecords AND ipcKeyFields EQ "" THEN DO:
    ipcKeyFields = "rec_key".
END.

IF ipcKeyFields NE "" THEN DO:
    DO iFieldCount = 1 TO NUM-ENTRIES(ipcKeyFields):
        FIND FIRST ASI._field NO-LOCK
             WHERE ASI._field._Field-Name = ENTRY(iFieldCount, ipcKeyFields)
               AND ASI._field._file-recid = RECID(ASI._file) NO-ERROR.
        IF NOT AVAILABLE ASI._field THEN DO:
            opcMessage = "Field '" + ENTRY(iFieldCount, ipcKeyFields) + "' is not available in " + ipcTable.
            RETURN.
        END.     
    END.
END.

/* Create database table buffer before temp-table-prepare */
CREATE BUFFER hdTableBuffer FOR TABLE ipcTable.

CREATE TEMP-TABLE hdTTSystemData.
hdTTSystemData:CREATE-LIKE (hdTableBuffer).
hdTTSystemData:TEMP-TABLE-PREPARE("tt" + ipcTable).

hdTTBuffer = hdTTSystemData:DEFAULT-BUFFER-HANDLE.

IF iplDeleteRecords THEN
    RUN pDeleteRecords.
    
RUN pImport.
    
IF VALID-HANDLE (hdTableBuffer) THEN
    DELETE OBJECT hdTableBuffer.

IF VALID-HANDLE (hdTTSystemData) THEN
    DELETE OBJECT hdTTSystemData.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pDeleteRecords:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdQuery AS HANDLE NO-UNDO.

    CREATE QUERY hdQuery.
    hdQuery:ADD-BUFFER (hdTableBuffer).
    hdQuery:QUERY-PREPARE ("FOR EACH " + hdTableBuffer:NAME + " NO-LOCK " + (IF ipcDeleteQuery NE "" THEN " WHERE " + ipcDeleteQuery ELSE "")).
    hdQuery:QUERY-OPEN ().
    
    hdQuery:GET-NEXT().
    
    DO WHILE NOT hdQuery:QUERY-OFF-END TRANSACTION:
        hdTableBuffer:FIND-CURRENT(EXCLUSIVE-LOCK).
                
        hdTableBuffer:BUFFER-DELETE (). 
    
        hdQuery:GET-NEXT().    
    END.
    
    hdQuery:QUERY-CLOSE ().

    IF VALID-HANDLE (hdQuery) THEN
        DELETE OBJECT hdQuery.
    
END PROCEDURE.

PROCEDURE pImport:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdQuery      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdTableQuery AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cQuery       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFieldCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hdField      AS HANDLE    NO-UNDO.
    
    lSuccess = hdTTSystemData:READ-JSON ("FILE", IF ipcFileName NE "" THEN ipcFileName ELSE ipcTable + ".json").
    
    IF NOT lSuccess THEN DO:
        opcMessage = "Error reading JSON file".
        RETURN.    
    END.
    
    CREATE QUERY hdQuery.
    hdQuery:ADD-BUFFER (hdTTBuffer).
    hdQuery:QUERY-PREPARE ("FOR EACH " + hdTTSystemData:NAME + " NO-LOCK " + (IF ipcWhereQuery NE "" THEN " WHERE " + ipcWhereQuery ELSE "")).
    hdQuery:QUERY-OPEN ().
    
    hdQuery:GET-NEXT().
    
    IF NOT iplDeleteRecords THEN DO:
        CREATE QUERY hdTableQuery.
        hdTableQuery:ADD-BUFFER (hdTableBuffer).
    END.
    
    DO WHILE NOT hdQuery:QUERY-OFF-END TRANSACTION:
        cQuery = "".
        IF NOT iplDeleteRecords AND ipcKeyFields NE "" THEN DO:
            cQuery = "FOR EACH " + hdTableBuffer:NAME + " WHERE TRUE ".  

            DO iFieldCount = 1 TO NUM-ENTRIES(ipcKeyFields):
                hdField = hdTableBuffer:BUFFER-FIELD (ENTRY(iFieldCount, ipcKeyFields)) NO-ERROR.
                IF VALID-HANDLE (hdField) THEN DO:
                    IF hdField:DATA-TYPE EQ "CHARACTER" THEN
                        cQuery = cQuery + " AND " + hdField:NAME + " EQ " + "'" + hdTTBuffer:BUFFER-FIELD(hdField:NAME):BUFFER-VALUE + "'".
                    ELSE
                        cQuery = cQuery + " AND " + hdField:NAME + " EQ " + hdTTBuffer:BUFFER-FIELD(hdField:NAME):BUFFER-VALUE.
                END. 
            END.

            hdTableQuery:QUERY-PREPARE (cQuery).
            hdTableQuery:QUERY-OPEN ().
            
            hdTableQuery:GET-FIRST().

            IF hdTableBuffer:AVAILABLE THEN DO:
                hdTableBuffer:FIND-CURRENT(EXCLUSIVE-LOCK).
                hdTableBuffer:BUFFER-COPY(hdTTBuffer).
            END.
            ELSE DO:
                hdTableBuffer:BUFFER-CREATE().
                hdTableBuffer:BUFFER-COPY(hdTTBuffer). 
            END.                 
            
            hdTableQuery:QUERY-CLOSE ().
        END.
        ELSE DO:            
            hdTableBuffer:BUFFER-CREATE().
            hdTableBuffer:BUFFER-COPY(hdTTBuffer). 
        END.
        
        hdQuery:GET-NEXT().    
    END.
    
    hdQuery:QUERY-CLOSE ().

    FILE-INFO:FILE-NAME = IF ipcFileName NE "" THEN ipcFileName ELSE ipcTable + ".json".
    
    opcMessage = "File imported from " + FILE-INFO:FULL-PATHNAME.  

    IF VALID-HANDLE (hdQuery) THEN
        DELETE OBJECT hdQuery.       
        
    IF VALID-HANDLE (hdTableQuery) THEN
        DELETE OBJECT hdTableQuery. 
END PROCEDURE.
