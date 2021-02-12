/*------------------------------------------------------------------------
  File:         qryRecKeyFix.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 11.6.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&Scoped-define excludeTables rec_key,reftable,reftable1,user-print,xUserMenu

/* Temp-Table Definitions ---                                           */

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD tableName   AS CHARACTER FORMAT "x(30)"     LABEL "Table Name"
    FIELD tableLabel  AS CHARACTER FORMAT "x(60)"     LABEL "Table Label"
    FIELD recKeyCount AS INTEGER   FORMAT ">,>>>,>>9" LABEL "Count"
        INDEX ttTempTable IS PRIMARY tableName
        . 

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 53
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLockType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hBuffer      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRecKeyCount AS INTEGER   NO-UNDO.
    
    cLockType = IF lCorrectData THEN "EXCLUSIVE" ELSE "NO".
    FOR EACH ASI._file NO-LOCK
        WHERE ASI._file._tbl-type EQ "T",
        FIRST ASI._field OF ASI._file NO-LOCK
        WHERE ASI._field._field-name EQ "rec_key"
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
        IF CAN-DO("{&excludeTables}",ASI._file._file-name) THEN
        NEXT.
        CREATE QUERY hQuery.
        CREATE BUFFER hBuffer FOR TABLE ASI._file._file-name.
        hQuery:ADD-BUFFER(hBuffer).
        hQuery:QUERY-PREPARE(
            "FOR EACH " + ASI._file._file-name + " " + cLockType + "-LOCK " +
            "WHERE " + ASI._file._file-name + ".rec_key EQ ~"~""
            ).
        hQuery:QUERY-OPEN().
        ASSIGN
            hTable = hQuery:GET-BUFFER-HANDLE(ASI._file._file-name)
            iRecKeyCount = 0
            .
        REPEAT TRANSACTION:
            hQuery:GET-NEXT().
            IF hQuery:QUERY-OFF-END THEN LEAVE.
            iRecKeyCount = iRecKeyCount + 1.
            IF lCorrectData THEN
            hTable:BUFFER-FIELD("rec_key"):BUFFER-VALUE() = DYNAMIC-FUNCTION("sfGetNextRecKey").
        END. /* repeat */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        DELETE OBJECT hTable.
        IF iRecKeyCount EQ 0 THEN NEXT.
        CREATE ttTempTable.
        ASSIGN
            ttTempTable.tableName   = ASI._file._file-name
            ttTempTable.tableLabe   = IF ASI._file._file-label EQ ? THEN ASI._file._file-name
                                      ELSE ASI._file._file-label
            ttTempTable.recKeyCount = iRecKeyCount
            .
    END. /* each _file */
END PROCEDURE.
