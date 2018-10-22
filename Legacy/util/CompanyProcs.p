
/*------------------------------------------------------------------------
    File        : CompanyProcs.p
    Purpose     : 

    Syntax      :

    Description : Houses all procedures related to processing data, en masse, for Multi-company utilities 
                (add, copy, merge)

    Author(s)   : BV
    Created     : Wed Oct 17 16:39:39 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\CompanyProcs.i}
DEFINE STREAM sTableList.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */


PROCEDURE GenerateTableListing:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeCounts AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
    
    RUN pBuildTableListing(iplIncludeCounts).
    OUTPUT STREAM sTableList TO VALUE(ipcFileName).
    
    DO iIndex = 1 TO TEMP-TABLE ttTables:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
        cFields = cFields + TEMP-TABLE ttTables:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):NAME + ",".
    END.
    PUT STREAM sTableList UNFORMATTED 
        TRIM(cFields, ",") SKIP.
    FOR EACH ttTables:
        EXPORT STREAM sTableList DELIMITER "," 
            ttTables
            .
    END. 

    OUTPUT STREAM sTableList CLOSE. 


END PROCEDURE.

PROCEDURE pBuildTableListing PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds ttTables for the DB Analysis
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplWithRecordCounts AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lCalculateRecordCounts AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iCount                 AS INTEGER NO-UNDO.
    DEFINE VARIABLE hQuery                 AS HANDLE  NO-UNDO. 
    DEFINE VARIABLE hBuffer                AS HANDLE  NO-UNDO.
    
    
    EMPTY TEMP-TABLE ttTables.
    FOR EACH _file NO-LOCK
        WHERE NOT _file._hidden:
        CREATE ttTables.
        ASSIGN 
            ttTables.cTable     = _file._file-name
            ttTables.cTableDesc = _file._desc
            .
        IF iplWithRecordCounts THEN 
        DO:
            RUN pCreateQuery(_file._file-name, "", YES, OUTPUT hQuery).
            IF VALID-HANDLE(hQuery) THEN 
                ttTables.iRecordCount = hQuery:NUM-RESULTS.
        END.    
        
        RUN pDoesFileHaveField(BUFFER _file, "company", OUTPUT ttTables.lHasCompany).
        RUN pDoesFileHaveField(BUFFER _file, "cust-no", OUTPUT ttTables.lHasCustID).
        RUN pDoesFileHaveField(BUFFER _file, "actnum", OUTPUT ttTables.lHasAcctNo).
        RUN pDoesFileHaveField(BUFFER _file, "i-no", OUTPUT ttTables.lHasINo).
        RUN pDoesFileHaveField(BUFFER _file, "rec_key", OUTPUT ttTables.lHasRecKey).
        RUN pDoesFileHaveField(BUFFER _file, "job-no", OUTPUT ttTables.lHasJobNo).
        RUN pDoesFileHaveField(BUFFER _file, "ord-no", OUTPUT ttTables.lHasOrderNo).
        RUN pDoesFileHaveField(BUFFER _file, "vend-no", OUTPUT ttTables.lHasVendID).
    END.
    
END PROCEDURE.

PROCEDURE pCreateQuery PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns handle to opened query
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWhere AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplNoLock AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER ophQuery AS HANDLE NO-UNDO.

    DEFINE VARIABLE cQuery  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO.

    cQuery = "PRESELECT EACH " + ipcTable.
    IF iplNoLock THEN 
        cQuery = cQuery + " NO-LOCK ".
    ELSE  
        cQuery = cQuery + " EXCLUSIVE-LOCK ".
    IF ipcWhere NE "" THEN 
        cQuery = cQuery + " " + ipcWhere.
    IF VALID-HANDLE(hBuffer) THEN DELETE WIDGET hBuffer.                                                          
    IF VALID-HANDLE(ophQuery) THEN DELETE WIDGET ophQuery.                                                
    CREATE BUFFER hBuffer FOR TABLE ipcTable.
    CREATE QUERY ophQuery.
    ophQuery:ADD-BUFFER(hBuffer).
    ophQuery:QUERY-PREPARE(cQuery).
    ophQuery:QUERY-OPEN().

END PROCEDURE.

PROCEDURE pDoesFileHaveField PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a _file buffer, return yes/no based on field name passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-file FOR _file.
    DEFINE INPUT PARAMETER ipcField AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplHasField AS LOGICAL NO-UNDO.

    FIND FIRST _field OF ipbf-file NO-LOCK 
        WHERE _field._field-name EQ ipcField
        NO-ERROR.
    oplHasField = AVAILABLE _field.

END PROCEDURE.

