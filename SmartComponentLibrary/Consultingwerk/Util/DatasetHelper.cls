/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *

 **********************************************************************/
/*------------------------------------------------------------------------
    File        : DatasetHelper
    Purpose     :
    Syntax      : Static methods only, private constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 14 08:37:47 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Assertion.*             FROM PROPATH .
USING Consultingwerk.Exceptions.*            FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Consultingwerk.Util.Enum.*             FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Util.DatasetHelper:

    /**
     * Purpose: Protected default constructor.
     * Notes:   There's no need to create instances of the helper classes
     */
    CONSTRUCTOR PROTECTED DatasetHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Adds an ERROR-STRING to a ProDataset Buffer, and sets the ERROR
     *          attribute of the BUFFER and the ProDataset
     * Notes:
     * @param phBuffer The ProDataset Member Buffer
     * @param pcErrorString The Error-String to add
     */
    METHOD PUBLIC STATIC VOID AddErrorString (phBuffer AS HANDLE,
                                              pcErrorString AS CHARACTER):

        DEFINE VARIABLE cErrorString AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .
        Consultingwerk.Assertion.BufferAssert:IsDatasetBuffer (phBuffer) .

        ASSIGN cErrorString = phBuffer:ERROR-STRING .

        IF CharacterType:IsNullOrEmpty (cErrorString) THEN
            ASSIGN cErrorString = pcErrorString .
        ELSE
            ASSIGN cErrorString = cErrorString +
                                  CHR(3) +
                                  pcErrorString .

        DatasetHelper:SetErrorString (phBuffer, cErrorString) .

    END METHOD.

    /**
     * Purpose: Adds an ERROR-STRING to a ProDataset Buffer, and sets the ERROR
     *          attribute of the BUFFER and the ProDataset
     * Notes:
     * @param phBuffer The ProDataset Member Buffer
     * @param pcErrorString The Error-String to add
     * @param pcErrorFieldName The name of the field this error message belongs to
     */
    METHOD PUBLIC STATIC VOID AddErrorString (phBuffer AS HANDLE,
                                              pcErrorString AS CHARACTER,
                                              pcErrorFieldName AS CHARACTER):

        DEFINE VARIABLE cErrorString AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .
        Consultingwerk.Assertion.BufferAssert:IsDatasetBuffer (phBuffer) .

        ASSIGN cErrorString = phBuffer:ERROR-STRING .

        IF CharacterType:IsNullOrEmpty (cErrorString) THEN
            ASSIGN cErrorString = SUBSTITUTE ("&1&4&2&4&3":U, pcErrorString, pcErrorFieldName, phBuffer:NAME, CHR (4)) .
        ELSE
            ASSIGN cErrorString = cErrorString +
                                  CHR(3) +
                                  SUBSTITUTE ("&1&4&2&4&3":U, pcErrorString, pcErrorFieldName, phBuffer:NAME, CHR (4)) .

        DatasetHelper:SetErrorString (phBuffer, cErrorString) .

    END METHOD.

    /**
     * Purpose: Adds an ERROR-STRING to a ProDataset Buffer, and sets the ERROR
     *          attribute of the BUFFER and the ProDataset
     * Notes:
     * @param phBuffer The ProDataset Member Buffer
     * @param pcErrorString The Error-String to add
     * @param pcErrorFieldName The name of the field this error message belongs to
     * @param pcErrorBufferName The name of the buffer this error message belongs to
     */
    METHOD PUBLIC STATIC VOID AddErrorString (phBuffer AS HANDLE,
                                              pcErrorString AS CHARACTER,
                                              pcErrorFieldName AS CHARACTER,
                                              pcErrorBufferName AS CHARACTER):

        DEFINE VARIABLE cErrorString AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .
        Consultingwerk.Assertion.BufferAssert:IsDatasetBuffer (phBuffer) .

        ASSIGN cErrorString = phBuffer:ERROR-STRING .

        IF CharacterType:IsNullOrEmpty (cErrorString) THEN
            ASSIGN cErrorString = SUBSTITUTE ("&1&4&2&4&3":U, pcErrorString, pcErrorFieldName, pcErrorBufferName, CHR (4)) .
        ELSE
            ASSIGN cErrorString = cErrorString +
                                  CHR(3) +
                                  SUBSTITUTE ("&1&4&2&4&3":U, pcErrorString, pcErrorFieldName, pcErrorBufferName, CHR (4)) .

        DatasetHelper:SetErrorString (phBuffer, cErrorString) .

    END METHOD.

    /**
     * Purpose: Returns a list of all buffer names in the ProDataset
     * Notes:
     * @param phDataset The handle of the ProDataset
     * @return The comma-delimited list of buffer names
     */
    METHOD PUBLIC STATIC CHARACTER BufferNames (phDataset AS HANDLE):

        DEFINE VARIABLE i     AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cList AS CHARACTER NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i  phDataset WidgetTypeEnum:Dataset} .

        DO i = 1 TO phDataset:NUM-BUFFERS:

            ASSIGN cList = cList + (IF cList > "":U THEN ",":U ELSE "":U) +
                           phDataset:GET-BUFFER-HANDLE (i):NAME .

        END.

        RETURN cList .

    END METHOD .

    /**
     * Purpose: Returns a comma-delimited list of the handles of the child buffers
     *          of a ProDataset buffer
     * Notes:
     * @param phBuffer The ProDataset buffer handle to return child buffers from
     * @return The comma-delimited list of the handles of the child buffers
     */
    METHOD PUBLIC STATIC CHARACTER ChildBuffers (phBuffer AS HANDLE):

        DEFINE VARIABLE hDataset     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cReturn      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
        ASSIGN hDataset = phBuffer:DATASET .

        IF NOT VALID-HANDLE (hDataset) OR hDataset:TYPE <> "DATASET":U THEN
            RETURN ERROR NEW AppError ("Only ProDataset Buffers can be used with the method ChildBuffers"{&TRAN}, 0) .

        DO i = 1 TO phBuffer:NUM-CHILD-RELATIONS:
            ASSIGN cReturn = cReturn + (IF i > 1 THEN ",":U ELSE "":U) +
                             STRING(phBuffer:GET-CHILD-RELATION(i):CHILD-BUFFER) .
        END.

        RETURN cReturn.

    END METHOD.

    /**
     * Purpose: Returns the query string matching a DATA-RELATION field pairs
     * Notes:   Uses the parent table field names in the query string
     * @param phBuffer The child buffer of the relation
     * @return The query string matching the data-relation criteria
     */
    METHOD PUBLIC STATIC CHARACTER ChildRelationQueryString (phBuffer AS HANDLE):

        DEFINE VARIABLE cQueryString  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hParentBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER   NO-UNDO .

        ASSIGN cQueryString = " EACH ":U + phBuffer:NAME + " WHERE ":U .

        ASSIGN hParentBuffer = DatasetHelper:ParentBuffer (phBuffer) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hParentBuffer """Parent Buffer Handle"":U"} .

        DO i = 1 TO NUM-ENTRIES(phBuffer:PARENT-RELATION:RELATION-FIELDS) BY 2:
            ASSIGN cQueryString = cQueryString +
                                  (IF i > 1 THEN " AND ":U ELSE "":U) +
                                  SUBSTITUTE("&1.&2 = &3.&4":U,
                                             phBuffer:NAME,
                                             ENTRY(i + 1, phBuffer:PARENT-RELATION:RELATION-FIELDS),
                                             hParentBuffer:NAME,
                                             ENTRY(i, phBuffer:PARENT-RELATION:RELATION-FIELDS)) .
        END.

        RETURN cQueryString .

    END METHOD.

    /**
     * Purpose: Returns the query string matching a DATA-RELATION field pairs
     * Notes:   Instead of the parent table field names, the current buffer values are
     *          used
     * @param phBuffer The child buffer of the relation
     * @return The query string matching the data-relation criteria
     */
    METHOD PUBLIC STATIC CHARACTER ChildRelationQueryStringWithValues (phBuffer AS HANDLE):

        DEFINE VARIABLE cQueryString  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hParentBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cField        AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER   NO-UNDO .

        BufferAssert:IsDatasetBuffer (phBuffer) .

        ASSIGN hParentBuffer = DatasetHelper:ParentBuffer (phBuffer) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hParentBuffer """Parent Buffer Handle"":U"} .
        BufferAssert:IsAvailable (hParentBuffer) .

        ASSIGN cQueryString = " EACH ":U + phBuffer:NAME + " WHERE ":U .

        DO i = 1 TO NUM-ENTRIES(phBuffer:PARENT-RELATION:RELATION-FIELDS) BY 2:
            ASSIGN cField       = ENTRY(i, phBuffer:PARENT-RELATION:RELATION-FIELDS)
                   cQueryString = cQueryString +
                                  (IF i > 1 THEN " AND ":U ELSE "":U) +
                                  SUBSTITUTE("&1.&2 = &3":U,
                                             phBuffer:NAME,
                                             ENTRY(i + 1, phBuffer:PARENT-RELATION:RELATION-FIELDS),
                                             QUOTER (STRING (hParentBuffer:BUFFER-FIELD (cField):BUFFER-VALUE))) .
        END.

        RETURN cQueryString .

    END METHOD.

    /**
     * Purpose: Copies the read-only tables (no before table) from the source dataset
     *          to the target dataset
     * Notes:   Useful in combination with the dataset MERGE-CHANGES method to copy
     *          also rows from tables without a before table form the source to the
     *          target dataset. Those tables are ignored by the MERGE-CHANGES method
     * @param phSourceDataset The handle of the source dataset
     * @param phTargetDataset The handle of the target dataset
     */
    METHOD PUBLIC STATIC VOID CopyReadOnlyTables (phSourceDataset AS HANDLE,
                                                  phTargetDataset AS HANDLE):

        DEFINE VARIABLE hBuffer  AS HANDLE  NO-UNDO .
        DEFINE VARIABLE hBuffer2 AS HANDLE  NO-UNDO .
        DEFINE VARIABLE hQuery   AS HANDLE  NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER NO-UNDO .

        bufferLoop:
        DO i = 1 TO phTargetDataset:NUM-BUFFERS ON ERROR UNDO, THROW:

            hBuffer = phTargetDataset:GET-BUFFER-HANDLE (i) .

            IF VALID-HANDLE (hBuffer:BEFORE-BUFFER) THEN
                NEXT bufferLoop .

            hBuffer2 = phSourceDataset:GET-BUFFER-HANDLE (hBuffer:NAME) .

            ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hBuffer2) .

            DO WHILE NOT hQuery:QUERY-OFF-END:
                hBuffer:FIND-UNIQUE (BufferHelper:UniqueFindPredicate (hBuffer2)) NO-ERROR .

                IF NOT hBuffer:AVAILABLE THEN
                    hBuffer:BUFFER-CREATE () .

                hBuffer:BUFFER-COPY (hBuffer2) .

                hQuery:GET-NEXT () .
            END.

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hQuery) .
            END FINALLY.
        END.

    END METHOD .

    /**
     * Purpose: Returns the RecordIdentifier identifying the first record in the
     *          ProDataset with Errors
     * Notes:   Processes the Dataset in the same order as DatasetErrorStrings
     * @param phDataset The handle of the dataset to return the error strings from
     * @return The RecordIdentifier identifying the first record in the ProDataset with errors
     */
    METHOD PUBLIC STATIC RecordIdentifier DatasetErrorRecordIdentifier (phDataset AS HANDLE):

        DEFINE VARIABLE iBuffer        AS INTEGER NO-UNDO.
        DEFINE VARIABLE hBeforeQuery   AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBeforeTable   AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBeforeBuffer  AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBeforeBuffer2 AS HANDLE NO-UNDO.

        CREATE QUERY hBeforeQuery.

        bufferLoop:
        DO iBuffer = 1 TO phDataset:NUM-BUFFERS:
           hBeforeTable = phDataset:GET-BUFFER-HANDLE(iBuffer):TABLE-HANDLE:BEFORE-TABLE.

           IF NOT VALID-HANDLE(hBeforeTable) OR NOT hBeforeTable:HAS-RECORDS THEN
               NEXT bufferLoop .

           hBeforeBuffer = phDataset:GET-BUFFER-HANDLE(iBuffer):BEFORE-BUFFER.
           hBeforeQuery:SET-BUFFERS (hBeforeBuffer).
           hBeforeQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH &1":U, hBeforeBuffer:NAME)).
           hBeforeQuery:QUERY-OPEN ().
           hBeforeQuery:GET-FIRST ().

           queryLoop:
           DO WHILE hBeforeQuery:QUERY-OFF-END = FALSE:

               IF hBeforeBuffer:ERROR OR hBeforeBuffer:REJECTED THEN
               DO:
                   IF hBeforeBuffer:ERROR-STRING > "":U THEN
                   DO:
                       IF VALID-HANDLE (hBeforeBuffer:AFTER-BUFFER:TABLE-HANDLE:ORIGIN-HANDLE) THEN DO:
                           hBeforeBuffer2 = hBeforeBuffer:TABLE-HANDLE:ORIGIN-HANDLE:DEFAULT-BUFFER-HANDLE:BEFORE-BUFFER .

                           hBeforeBuffer2:FIND-BY-ROWID (hBeforeBuffer:ORIGIN-ROWID) NO-ERROR .

                           IF NOT hBeforeBuffer2:AVAILABLE THEN DO:
                               hBeforeQuery:GET-NEXT() .
                               NEXT queryLoop .
                           END.

                           RETURN NEW RecordIdentifier (hBeforeBuffer:AFTER-BUFFER:NAME,
                                                        hBeforeBuffer2:AFTER-ROWID,
                                                        hBeforeBuffer:ROW-STATE) .
                       END .
                       ELSE
                           RETURN NEW RecordIdentifier (hBeforeBuffer:AFTER-BUFFER:NAME,
                                                        hBeforeBuffer:AFTER-ROWID,
                                                        hBeforeBuffer:ROW-STATE) .
                   END.
               END.

               hBeforeQuery:GET-NEXT() .
           END.
        END.

        RETURN ? .

        FINALLY:
            DELETE OBJECT hBeforeQuery.
        END FINALLY.
    END METHOD .

    /**
     * Purpose: Returns a single CHARACTER string composed of the error-strings
     *          of all records (from all tables in the given Dataset instance)
     * Notes:
     * @param phDataset The handle of the dataset to return the error strings from
     * @return The string composed of the error-strings of all records
     */
    METHOD PUBLIC STATIC CHARACTER DatasetErrorStrings (phDataset AS HANDLE):

        DEFINE VARIABLE cErrorStrings AS CHARACTER NO-UNDO.

        DEFINE VARIABLE iBuffer       AS INTEGER NO-UNDO.
        DEFINE VARIABLE hBeforeQuery  AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBeforeTable  AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBeforeBuffer AS HANDLE  NO-UNDO.

        CREATE QUERY hBeforeQuery.

        buferLoop:
        DO iBuffer = 1 TO phDataset:NUM-BUFFERS:

           hBeforeTable = phDataset:GET-BUFFER-HANDLE(iBuffer):TABLE-HANDLE:BEFORE-TABLE.

           IF NOT VALID-HANDLE(hBeforeTable) OR NOT hBeforeTable:HAS-RECORDS THEN
               NEXT buferLoop .

           hBeforeBuffer = phDataset:GET-BUFFER-HANDLE(iBuffer):BEFORE-BUFFER.
           hBeforeQuery:SET-BUFFERS (hBeforeBuffer).
           hBeforeQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH &1":U, hBeforeBuffer:NAME)).
           hBeforeQuery:QUERY-OPEN ().
           hBeforeQuery:GET-FIRST ().

           DO WHILE hBeforeQuery:QUERY-OFF-END = FALSE:

               IF hBeforeBuffer:ERROR OR hBeforeBuffer:REJECTED THEN
               DO:
                   IF hBeforeBuffer:ERROR-STRING > "":U THEN
                   DO:
                       cErrorStrings = ErrorHelper:AddErrorMessage (cErrorStrings,
                                                                    hBeforeBuffer:ERROR-STRING,
                                                                    "":U,
                                                                    phDataset:GET-BUFFER-HANDLE(iBuffer):TABLE-HANDLE:NAME).
                   END.
               END.

               hBeforeQuery:GET-NEXT() .
           END.
        END.

        RETURN cErrorStrings .

        FINALLY:
            DELETE OBJECT hBeforeQuery.
        END FINALLY.
    END METHOD .

    /**
     * Purpose: Deletes the record from the given ProDataset buffer and recursively
     *          deletes all child records
     * Notes:
     * @param phBuffer The handle of the ProDataset buffer with the record to delete
     */
    METHOD PUBLIC STATIC VOID DeleteCascade (phBuffer AS HANDLE):

        DEFINE VARIABLE i            AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hChildBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cQuery       AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO .

        BufferAssert:IsDatasetBuffer (phBuffer) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .

        DO i = 1 TO phBuffer:NUM-CHILD-RELATIONS ON ERROR UNDO, THROW:
            ASSIGN cQuery = DatasetHelper:ChildRelationQueryStringWithValues(phBuffer:GET-CHILD-RELATION (i):CHILD-BUFFER) .

            hChildBuffer = phBuffer:GET-CHILD-RELATION (i):CHILD-BUFFER .

            hQuery = QueryHelper:CreatePreparedQuery (hChildBuffer, "FOR ":U + cQuery) .

            DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
                DatasetHelper:DeleteCascade (hChildBuffer) .

                hQuery:GET-NEXT () .
            END.

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hQuery) .
            END FINALLY.
        END.

        phBuffer:BUFFER-DELETE () .

    END METHOD .

    /**
     * Purpose: Deletes a ProDataset object handle and all it's buffers
     * Notes:   Useful, when dynamic buffers have been added to a ProDataset manually
     *          any are not maintained by the ProDataset (like they would be when the
     *          Dataset was created by the CREATE-LIKE method or similar).
     * @param phDataset The handle of the ProDataset
     */
    METHOD PUBLIC STATIC VOID DeleteDatasetAndBuffers (phDataset AS HANDLE):

        DEFINE VARIABLE hBuffers AS HANDLE  NO-UNDO EXTENT .
        DEFINE VARIABLE i        AS INTEGER NO-UNDO.
        DEFINE VARIABLE iCount   AS INTEGER NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i  phDataset WidgetTypeEnum:Dataset} .

        ASSIGN hBuffers = DatasetHelper:GetBufferHandles (phDataset) .

        DELETE OBJECT phDataset .

        ASSIGN iCount = EXTENT (hBuffers) .

        DO i = 1 TO iCount:
            IF VALID-HANDLE (hBuffers[i]) AND hBuffers[i]:DYNAMIC THEN
                DELETE OBJECT hBuffers[i] .
        END.

    END METHOD.

    /**
     * Purpose: Deletes the record from the given ProDataset buffer only when
     *          there is no child record
     * Notes:   Throws an error message, when a child error still exists
     * @param phBuffer The handle of the ProDataset buffer with the record to delete
     */
    METHOD PUBLIC STATIC VOID DeleteRestrict (phBuffer AS HANDLE):

        DEFINE VARIABLE i            AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hChildBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cQuery       AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO .

        BufferAssert:IsDatasetBuffer (phBuffer) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .

        DO i = 1 TO phBuffer:NUM-CHILD-RELATIONS ON ERROR UNDO, THROW:

            ASSIGN cQuery = DatasetHelper:ChildRelationQueryStringWithValues(phBuffer:GET-CHILD-RELATION (i):CHILD-BUFFER) .

            CREATE BUFFER hChildBuffer FOR TABLE phBuffer:GET-CHILD-RELATION (i):CHILD-BUFFER .

            hQuery = QueryHelper:CreatePreparedQuery (hChildBuffer, "FOR ":U + cQuery) .

            IF hChildBuffer:AVAILABLE THEN
                UNDO, THROW NEW ReferentialIntegrityException (SUBSTITUTE ("Unable to delete &1 because &2 exists."{&TRAN},
                                                                           phBuffer:NAME,
                                                                           hChildBuffer:NAME), 0) .

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hQuery) .
                GarbageCollectorHelper:DeleteObject (hChildBuffer) .
            END FINALLY.
        END.

        phBuffer:BUFFER-DELETE () .

    END METHOD .

    /**
     * Purpose: Empties a ProDataset
     * Notes:   Assigns Tracking-Changes of all temp-tables to FALSE prior to
     *          emptying the ProDataset and restores the state of Tracking-Changes
     *          afterwards
     * @param phDataset The handle of the ProDataset
     */
    METHOD PUBLIC STATIC VOID EmptyDataset (phDataset AS HANDLE):

        DEFINE VARIABLE lTrackingChanges AS LOGICAL NO-UNDO EXTENT .
        DEFINE VARIABLE i                AS INTEGER NO-UNDO .

        HandleAssert:WidgetType(phDataset, WidgetTypeEnum:Dataset) .

        ASSIGN EXTENT (lTrackingChanges) = phDataset:NUM-BUFFERS .

        DO i = 1 TO phDataset:NUM-BUFFERS:
            IF VALID-HANDLE (phDataset:GET-BUFFER-HANDLE(i):BEFORE-BUFFER) THEN
                ASSIGN lTrackingChanges[i]                                          = phDataset:GET-BUFFER-HANDLE(i):TABLE-HANDLE:TRACKING-CHANGES
                       phDataset:GET-BUFFER-HANDLE(i):TABLE-HANDLE:TRACKING-CHANGES = FALSE .
        END.

        phDataset:EMPTY-DATASET () .

        DO i = 1 TO phDataset:NUM-BUFFERS:
            IF VALID-HANDLE (phDataset:GET-BUFFER-HANDLE(i):BEFORE-BUFFER) THEN
                ASSIGN phDataset:GET-BUFFER-HANDLE(i):TABLE-HANDLE:TRACKING-CHANGES = lTrackingChanges[i] .
        END.

    END METHOD.

    /**
     * Purpose: Find the first buffer of the list in the given ProDataset that
     *          contains a field with the given name
     * Notes:   Returns ? when the field is not available in any buffer
     * @param phDataset The handle of the ProDataset
     * @param pcBufferNames The comma delimited list of buffer names of the ProDataset
     * @param pcFieldName The unqualified name of the field to locate
     * @return The handle of the first buffer that contains the field
     */
    METHOD PUBLIC STATIC HANDLE FindBufferWithField (phDataset AS HANDLE,
                                                     pcBufferNames AS CHARACTER,
                                                     pcFieldName AS CHARACTER):

        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cBufferName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO .

        DO i = 1 TO NUM-ENTRIES (pcBufferNames):

            ASSIGN cBufferName = ENTRY (i, pcBufferNames) .

            DatasetAssert:HasBuffer (phDataset, cBufferName) .

            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (cBufferName) .

            IF BufferHelper:HasField (hBuffer, pcFieldName) THEN
                RETURN hBuffer .
        END.

        RETURN ? .

    END METHOD .

    /**
     * Purpose: Returns an Array of the Buffer handles of the given Dataset
     * Notes:   Returns an uninitialized array and not a runtime error when the dataset
     *          does not contain any buffer
     * @param phDataset The handle of the dataset
     * @return The Array of Buffer handles
     */
    METHOD PUBLIC STATIC HANDLE EXTENT GetBufferHandles (phDataset AS HANDLE):

        DEFINE VARIABLE hBuffer AS HANDLE  NO-UNDO EXTENT.
        DEFINE VARIABLE iCount  AS INTEGER NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        IF phDataset:NUM-BUFFERS = 0 THEN
            RETURN hBuffer . /* return uninitialized Array */

        ASSIGN iCount           = phDataset:NUM-BUFFERS
               EXTENT (hBuffer) = iCount .

        DO i = 1 TO iCount:
            ASSIGN hBuffer[i] = phDataset:GET-BUFFER-HANDLE (i) .
        END .

        RETURN hBuffer.

    END METHOD.

    /**
     * Purpose: Returns the index of the buffer name in the ProDataset
     * Notes:   Return ? when the buffer is not a member of the ProDatast
     * @param phDataset The handle of the ProDataset
     * @param pcBufferName The name of the buffer to locate
     * @return The index of the buffer in the ProDataset buffer's list
     */
    METHOD PUBLIC STATIC INTEGER GetBufferIndex (phDataset AS HANDLE,
                                                 pcBufferName AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        HandleAssert:ValidHandle (phDataset, "phDataset":U) .
        Assert:NotNullOrEmpty (pcBufferName, "pcBufferName":U) .

        DO i = 1 TO phDataset:NUM-BUFFERS:
            IF phDataset:GET-BUFFER-HANDLE (i):NAME = pcBufferName THEN
                RETURN i .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Builds a dataset containing the changes records from the
     *          source dataset.
     * Notes:   This method returns a dataset, clean up has to be done by the
     *          caller.
     * @param phDataset The handle of the original dataset
     * @return The handle of a new dataset containing the changes from the original dataset
     */
    METHOD PUBLIC STATIC HANDLE GetChangesDataset (phDataset AS HANDLE):

        DEFINE VARIABLE hChangesDataset AS HANDLE NO-UNDO.

        CREATE DATASET hChangesDataset .
        hChangesDataset:CREATE-LIKE (phDataset /*, "ch_"*/ ) .
        hChangesDataset:GET-CHANGES (phDataset, TRUE) .

        RETURN hChangesDataset .

    END METHOD.

    /**
     * Purpose: Returns the handle of the child relation of a buffer by name
     * Notes:
     * @param phParentBuffer The handle to the parent buffer
     * @param pcDataRelationName The name of the data-relation to return
     * @return The handle of the data-relation with the matching name
     */
    METHOD PUBLIC STATIC HANDLE GetChildRelation (phParentBuffer AS HANDLE,
                                                  pcDataRelationName AS CHARACTER):

        DEFINE VARIABLE i             AS INTEGER NO-UNDO.
        DEFINE VARIABLE hDataRelation AS HANDLE  NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phParentBuffer ""BUFFER"":U} .

        DO i = 1 TO phParentBuffer:NUM-CHILD-RELATIONS:
            ASSIGN hDataRelation = phParentBuffer:GET-CHILD-RELATION (i) .

            IF hDataRelation:NAME = pcDataRelationName THEN
                RETURN hDataRelation .

            ASSIGN hDataRelation = ? .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Returns an array with the current ROWID's of every ProDataset buffer
     * Notes:   Useful in situations where ProDataset methods such as GET-CHANGES
     *          or MERGE-CHANGES invalidate the current record buffers
     * @param phDataset The handle of the ProDataset
     * @return The variable extent array of ROWID's matching the order of buffers in the ProDataset
     */
    METHOD PUBLIC STATIC ROWID EXTENT GetDatasetRowids (phDataset AS HANDLE):

        DEFINE VARIABLE roRowid AS ROWID EXTENT NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER      NO-UNDO .
        DEFINE VARIABLE hBuffer AS HANDLE       NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        EXTENT (roRowid) = phDataset:NUM-BUFFERS .

        DO i = 1 TO phDataset:NUM-BUFFERS:
            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (i) .

            IF hBuffer:AVAILABLE THEN
                roRowid[i] = hBuffer:ROWID .
            ELSE
                hBuffer = ? .
        END.

        RETURN roRowid .

    END METHOD .

    /**
     * Purpose: Creates a Dataset as a copy of another Dataset that only contains
     *          the current record in the given ProDataset temp-table buffer
     * Notes:   Useful to send individual records of a ProDataset to Business Entity
     *          InvokeMethod or Business Task methods
     * @param phBuffer The handle of the ProDataset Buffer that contains the record to be copied
     * @return The copy of the ProDataset, the caller is responsible to clean this up when no longer needed
     */
    METHOD PUBLIC STATIC HANDLE GetDatasetWithCurrentRecord (phBuffer AS HANDLE):

        DEFINE VARIABLE hSourceDataset AS HANDLE NO-UNDO .
        DEFINE VARIABLE hDataset       AS HANDLE NO-UNDO .
        DEFINE VARIABLE hCopyBuffer    AS HANDLE NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .
        BufferAssert:IsDatasetBuffer (phBuffer) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .

        hSourceDataset = phBuffer:DATASET .

        CREATE DATASET hDataset.
        hDataset:CREATE-LIKE (hSourceDataset) .

        hCopyBuffer = hDataset:GET-BUFFER-HANDLE (phBuffer:NAME) .

        hCopyBuffer:BUFFER-CREATE () .
        hCopyBuffer:BUFFER-COPY (phBuffer) .

        RETURN hDataset .

    END METHOD.

    /**
     * Purpose: Returns an array of the Query handles of the Data-Sources of the
     *          given Dataset
     * Notes:   Array may contain invalid ? entries when data sources don't have a
     *          query assigned
     * @param phDataset The handle of the ProDataset
     * @return The handle array with the handles of the data-source queries
     */
    METHOD PUBLIC STATIC HANDLE EXTENT GetDataSourceQueryHandles (phDataset AS HANDLE):

        DEFINE VARIABLE hQueries AS HANDLE  NO-UNDO EXTENT .
        DEFINE VARIABLE iBuffer  AS INTEGER NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        ASSIGN EXTENT (hQueries) = phDataset:NUM-BUFFERS .

        DO iBuffer = 1 TO phDataset:NUM-BUFFERS ON ERROR UNDO, THROW:
            IF VALID-HANDLE (phDataset:GET-BUFFER-HANDLE (iBuffer):DATA-SOURCE) THEN
                hQueries [iBuffer] = phDataset:GET-BUFFER-HANDLE (iBuffer):DATA-SOURCE:QUERY .

            CATCH err AS Progress.Lang.SysError:
                /* Ignore ** Failed to auto-prepare an automatic fill query. (11981) */
                IF NOT ErrorHelper:HasMessageNumber (err, 11981) THEN
                    UNDO, THROW err .
            END CATCH.
        END.

        RETURN hQueries .

    END METHOD .

    /**
     * Purpose: Returns an Array of the Temp-Table handles of the given Dataset
     * Notes:   Returns an uninitialized array and not a runtime error when the dataset
     *          does not contain any buffer
     * @param phDataset The handle of the dataset
     * @return The Array of Temp-Table handles
     */
    METHOD PUBLIC STATIC HANDLE EXTENT GetTableHandles (phDataset AS HANDLE):

        DEFINE VARIABLE hTable  AS HANDLE  NO-UNDO EXTENT.
        DEFINE VARIABLE iCount  AS INTEGER NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        IF phDataset:NUM-BUFFERS = 0 THEN
            RETURN hTable . /* return uninitialized Array */

        ASSIGN iCount           = phDataset:NUM-BUFFERS
               EXTENT (hTable) = iCount .

        DO i = 1 TO iCount:
            ASSIGN hTable[i] = phDataset:GET-BUFFER-HANDLE (i):TABLE-HANDLE .
        END .

        RETURN hTable .

    END METHOD.

    /**
     * Purpose: Returns if the Dataset is currently TRACKING-CHANGES
     * Notes:   Returns the value of the TRACKING-CHANGES attribute of the first
     *          updatable temp-table. When there is not updatable temp-table, FALSE
     *          will be returned
     * @param phDataset Handle of the dataset
     * @return Logical value indicating if the Dataset is TRACKING-CHANGES
     */
    METHOD PUBLIC STATIC LOGICAL GetTrackingChanges (phDataset AS HANDLE):

        DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer       AS HANDLE    NO-UNDO.

        IF NOT VALID-HANDLE (phDataset) THEN
            RETURN ? .

        DO i = 1 TO phDataset:NUM-BUFFERS:
            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE(i) NO-ERROR .

            IF VALID-HANDLE(hBuffer:BEFORE-BUFFER) THEN
                RETURN hBuffer:TABLE-HANDLE:TRACKING-CHANGES .
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Returns if the the passed in ProDataset handle has a buffer with the
     *          given name
     * Notes:
     * @param phDataset The handle of the ProDataset
     * @param pcBufferName The name of the Buffer that should exist
     * @return Logical value indicating if the ProDataset has a buffer with the given name
     */
    METHOD PUBLIC STATIC LOGICAL HasBuffer (phDataset AS HANDLE,
                                            pcBufferName AS CHARACTER):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        hBuffer = phDataset:GET-BUFFER-HANDLE (pcBufferName) NO-ERROR .

        RETURN VALID-HANDLE (hBuffer) .

        FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Tests if a ProDataset has changes
     * Notes:   Similar to the System.Data.Dataset.HasChanges attribute
     * @param phDataset Handle of the dataset
     * @return Logical value indicating if the ProDataset has change records (before table records)
     */
    METHOD PUBLIC STATIC LOGICAL HasChanges (phDataset AS HANDLE):

        DEFINE VARIABLE i      AS INTEGER NO-UNDO.
        DEFINE VARIABLE hTable AS HANDLE  NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset ""DATASET"":U} .

        DO i = 1 TO phDataset:NUM-BUFFERS:

            ASSIGN hTable = phDataset:GET-BUFFER-HANDLE (i):TABLE-HANDLE .

            IF VALID-HANDLE (hTable:BEFORE-TABLE) AND hTable:BEFORE-TABLE:HAS-RECORDS THEN
                RETURN TRUE .
        END .

        RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Returns if the provided data-relation is pointing to an unique
     *          child record
     * Notes:   Returns TRUE for eOrder->eCustomer and FALSE for eOrder->eOrderLine
     * @param phRelation The handle of the DATA-RELATION to test
     * @return Logical value indicating if the data-relation is pointing to an unique child record
     */
    METHOD PUBLIC STATIC LOGICAL IsUniqueChildRelation (phRelation AS HANDLE):

        DEFINE VARIABLE cFields AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO .

        HandleAssert:WidgetType (phRelation, WidgetTypeEnum:DataRelation) .

        DO i = 2 TO NUM-ENTRIES (phRelation:RELATION-FIELDS) BY 2:
            ASSIGN cFields = cFields + ENTRY (i, phRelation:RELATION-FIELDS) + ",":U .
        END.

        RETURN BufferHelper:IsUniqueFieldCombination (phRelation:CHILD-BUFFER, TRIM (cFields, ",":U)) .

    END METHOD.

    /**
     * Purpose: Returns the handle of the parent buffer of a ProDataset buffer
     * Notes:
     * @param phBuffer The handle of the ProDataset buffer (child buffer)
     * @return The handle of the parent buffer
     */
    METHOD PUBLIC STATIC HANDLE ParentBuffer (phBuffer AS HANDLE):

        DEFINE VARIABLE hDataset     AS HANDLE    NO-UNDO.

        ASSIGN hDataset = phBuffer:DATASET .

        IF NOT VALID-HANDLE (hDataset) OR hDataset:TYPE <> "DATASET":U THEN
            RETURN ERROR NEW AppError ("Only ProDataset Buffers can be used with the method ChildBuffers."{&TRAN}, 0) .

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2009
           Gracefully ignore buffers with no parent relation */
        IF NOT VALID-HANDLE (phBuffer:PARENT-RELATION) THEN
            RETURN ? .

        RETURN phBuffer:PARENT-RELATION:PARENT-BUFFER .

    END METHOD.

    /**
     * Purpose: Returns a RowidDictionary with the ROWID's of the current record as
     *          well as the parent and grand-parent ... records based on the ProDataset
     *          relations
     * Notes:
     * @param phBuffer The handle of the child buffer to return the parent ROWID's for
     * @return The RowidDictionary with the ROWID's of the parent records
     */
    METHOD PUBLIC STATIC RowidDictionary ParentRecordRowids (phBuffer AS HANDLE):

        DEFINE VARIABLE oDictionary   AS RowidDictionary NO-UNDO .
        DEFINE VARIABLE hParentBuffer AS HANDLE          NO-UNDO.

        BufferAssert:IsDatasetBuffer (phBuffer) .
        BufferAssert:IsAvailable (phBuffer) .

        oDictionary = NEW RowidDictionary () .

        oDictionary:Add (phBuffer:NAME, phBuffer:ROWID) .

        DO WHILE VALID-HANDLE (phBuffer:PARENT-RELATION):

            ASSIGN hParentBuffer = phBuffer:PARENT-RELATION:PARENT-BUFFER .

            hParentBuffer:FIND-FIRST (DatasetHelper:ParentRelationWhereStringWithValues(phBuffer)) .

            phBuffer = hParentBuffer .

            oDictionary:Add (hParentBuffer:NAME, hParentBuffer:ROWID) .
        END.

        RETURN oDictionary.

    END METHOD.

    /**
     * Purpose: Returns the where string matching a DATA-RELATION field pairs
     * Notes:   Instead of the parent table field names, the current buffer values of
     *          the child table are used in the where string
     * @param phBuffer The child buffer of the relation
     * @return The query string matching the data-relation criteria
     */
    METHOD PUBLIC STATIC CHARACTER ParentRelationWhereStringWithValues (phBuffer AS HANDLE):

        DEFINE VARIABLE cWhereString  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hParentBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cField        AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER   NO-UNDO .

        BufferAssert:IsDatasetBuffer (phBuffer) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phBuffer:PARENT-RELATION """Parent DATA-RELATION"":U"} .

        ASSIGN hParentBuffer = phBuffer:PARENT-RELATION:PARENT-BUFFER .

        BufferAssert:IsAvailable (phBuffer) .

        ASSIGN cWhereString = "WHERE ":U .

        DO i = 1 TO NUM-ENTRIES(phBuffer:PARENT-RELATION:RELATION-FIELDS) BY 2:
            ASSIGN cField       = ENTRY (i, phBuffer:PARENT-RELATION:RELATION-FIELDS)
                   cWhereString = cWhereString  +
                                  (IF i > 1 THEN " AND ":U ELSE "":U) +
                                  SUBSTITUTE("&1.&2 = &3":U,
                                             hParentBuffer:NAME,
                                             cField,
                                             QUOTER (STRING (phBuffer:BUFFER-FIELD (ENTRY(i + 1, phBuffer:PARENT-RELATION:RELATION-FIELDS)):BUFFER-VALUE))) .
        END.

        RETURN cWhereString .

    END METHOD.

    /**
     * Purpose: Rejects all row-changes in the ProDatasets before tables that
     *          do not have the row-mod of Adding
     * Notes:   ROW-UNMODIFIED  0  The row was not modified.
     *          ROW-DELETED  1  The row was deleted.
     *          ROW-MODIFIED  2  The row was modified.
     *          ROW-CREATED  3  The row was created.
     * @param phDataset The handle to the dataset
     */
    METHOD PUBLIC STATIC VOID RejectNonCreateChanges (phDataset AS HANDLE):

        DEFINE VARIABLE iBuffer       AS INTEGER NO-UNDO.
        DEFINE VARIABLE hBeforeQuery  AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBeforeTable  AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBeforeBuffer AS HANDLE  NO-UNDO.

        CREATE QUERY hBeforeQuery.

        buferLoop:
        DO iBuffer = 1 TO phDataset:NUM-BUFFERS:
           hBeforeTable = phDataset:GET-BUFFER-HANDLE(iBuffer):TABLE-HANDLE:BEFORE-TABLE.

           IF NOT VALID-HANDLE(hBeforeTable) OR NOT hBeforeTable:HAS-RECORDS THEN
               NEXT buferLoop.

           hBeforeBuffer = phDataset:GET-BUFFER-HANDLE(iBuffer):BEFORE-BUFFER.
           hBeforeQuery:SET-BUFFERS (hBeforeBuffer).
           hBeforeQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH &1":U, hBeforeBuffer:NAME)).
           hBeforeQuery:QUERY-OPEN ().
           hBeforeQuery:GET-FIRST ().

           DO WHILE hBeforeQuery:QUERY-OFF-END = FALSE.
               IF hBeforeBuffer:ROW-STATE = ROW-DELETED OR hBeforeBuffer:ROW-STATE = ROW-MODIFIED THEN
                   hBeforeBuffer:REJECT-ROW-CHANGES () .

               hBeforeQuery:GET-NEXT() .
           END.
        END.

        FINALLY:
            DELETE OBJECT hBeforeQuery.
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Locates the records in all ProDataset buffers based on an Array
     *          of ROWID's
     * Notes:   The input parameter proRowids can be obtained from the GetDatasetRowids
     *          method
     * @param phDataset The handle of the ProDataset
     * @param proRowids The variable extent array of ROWID's matching the order of buffers in the ProDataset
     */
    METHOD PUBLIC STATIC VOID RepositionDatasetBuffers (phDataset AS HANDLE,
                                                        proRowids AS ROWID EXTENT):

        DEFINE VARIABLE i       AS INTEGER      NO-UNDO .
        DEFINE VARIABLE hBuffer AS HANDLE       NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        DO i = 1 TO phDataset:NUM-BUFFERS:
            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (i) .

            IF proRowids[i] <> ? THEN
                hBuffer:FIND-BY-ROWID (proRowids[i]) NO-ERROR .
            ELSE
                hBuffer:BUFFER-RELEASE () .
        END.

    END METHOD .

    /**
     * Purpose: Assigns the Query handles of the data-source widgets attached to the
     *          ProDataset buffers
     * Notes:   Ignores Cannot assign auto-generated FILL query to user FILL query. (11866)
     *          in which case it seems safe to assign ? to the DATA-SOURCE QUERY attribute
     * @param phDataset The handle of the ProDataset
     * @param phQueries The array of the query handles to assign to the data-source queries
     */
    METHOD PUBLIC STATIC VOID SetDataSourceQueryHandles (phDataset AS HANDLE,
                                                         phQueries AS HANDLE EXTENT):

        DEFINE VARIABLE iBuffer  AS INTEGER NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        DO iBuffer = 1 TO phDataset:NUM-BUFFERS ON ERROR UNDO, THROW:
            IF VALID-HANDLE (phDataset:GET-BUFFER-HANDLE (iBuffer):DATA-SOURCE) THEN DO:
                IF VALID-HANDLE (phQueries [iBuffer]) THEN
                    phDataset:GET-BUFFER-HANDLE (iBuffer):DATA-SOURCE:QUERY = phQueries [iBuffer] .
                ELSE
                    phDataset:GET-BUFFER-HANDLE (iBuffer):DATA-SOURCE:QUERY = ? .
            END.

            CATCH err AS Progress.Lang.SysError:
                IF err:NumMessages > 0 AND err:GetMessageNum (1) = 11866 THEN
                   ASSIGN phDataset:GET-BUFFER-HANDLE (iBuffer):DATA-SOURCE:QUERY = ? .
                ELSE
                    UNDO, THROW err .
            END CATCH.
        END.

    END METHOD .

    /**
     * Purpose: Sets the ERROR-STRING of a ProDataset Buffer, and the ERROR attribute
     *          of the BUFFER and the ProDataset
     * Notes:
     * @param phBuffer The ProDataset Member Buffer
     * @param pcErrorString The Error-String
     */
    METHOD PUBLIC STATIC VOID SetErrorString (phBuffer AS HANDLE,
                                              pcErrorString AS CHARACTER):

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .
        Consultingwerk.Assertion.BufferAssert:IsDatasetBuffer (phBuffer) .

        ASSIGN phBuffer:ERROR-STRING  = pcErrorString
               phBuffer:ERROR         = TRUE
               phBuffer:DATASET:ERROR = TRUE .

    END METHOD.

    /**
     * Purpose: Sets the tracking changes property of all temp-tables of the passed
     *          in dataset
     * Notes:   Tables without a valid BEFORE-BUFFER will not be handled. For static
     *          temp-tables this is typically understood as read-only temp-tables
     *          (and the attempt to enable TRACKING-CHANGES would cause a runtime
     *          error). Dynamic temp-tables that have never been set to
     *          TRACKING-CHANGES = TRUE will never have a valid BEFORE-BUFFER. As
     *          such it's expected that dynamic temp-tables that should be updatable
     *          will be set to TRACKING-CHANGES briefly in their factory method.
     * @param phDataset The handle to the Dataset
     * @param plTrackingChanges LOGICAL value indicating if tracking changes should be turned on or off
     */
    METHOD PUBLIC STATIC VOID SetTrackingChanges (phDataset AS HANDLE,
                                                  plTrackingChanges AS LOGICAL):

        DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer       AS HANDLE    NO-UNDO.

        IF NOT VALID-HANDLE (phDataset) THEN
            RETURN .

        Assert:NotNull (plTrackingChanges) .

        DO i = 1 TO phDataset:NUM-BUFFERS:
            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE(i) NO-ERROR .

            IF VALID-HANDLE(hBuffer:BEFORE-BUFFER) THEN
                hBuffer:TABLE-HANDLE:TRACKING-CHANGES = plTrackingChanges .
        END.

    END METHOD.

    /**
     * Purpose: Sets the tracking changes property of all temp-tables of the passed
     *          in dataset
     * Notes:   Tables without a valid BEFORE-BUFFER will not be handled. For static
     *          temp-tables this is typically understood as read-only temp-tables
     *          (and the attempt to enable TRACKING-CHANGES would cause a runtime
     *          error). Dynamic temp-tables that have never been set to
     *          TRACKING-CHANGES = TRUE will never have a valid BEFORE-BUFFER.
     *          The poDynamicTempTables parameter allows to indicate how to handle
     *          dynamic temp-tables
     * @param phDataset The handle to the Dataset
     * @param plTrackingChanges LOGICAL value indicating if tracking changes should be turned on or off
     * @param poDynamicTempTables The DynamicTempTablesEnum value indicating how to handle dynamic tmep-tables
     */
    METHOD PUBLIC STATIC VOID SetTrackingChanges (phDataset AS HANDLE,
                                                  plTrackingChanges AS LOGICAL,
                                                  poDynamicTempTables AS DynamicTempTablesEnum):

        DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer       AS HANDLE    NO-UNDO.

        IF NOT VALID-HANDLE (phDataset) THEN
            RETURN .

        Assert:NotNull (plTrackingChanges) .
        ObjectAssert:IsValid(poDynamicTempTables, "poDynamicTempTables":U) .

        IF poDynamicTempTables = DynamicTempTablesEnum:Ignore THEN DO:
            DatasetHelper:SetTrackingChanges(phDataset, plTrackingChanges) .
            RETURN .
        END.

        DO i = 1 TO phDataset:NUM-BUFFERS:
            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE(i) NO-ERROR .

            IF VALID-HANDLE(hBuffer:BEFORE-BUFFER) OR hBuffer:TABLE-HANDLE:DYNAMIC = TRUE THEN
                hBuffer:TABLE-HANDLE:TRACKING-CHANGES = plTrackingChanges .
        END.

    END METHOD.

    /**
     * Purpose: Throws Dataset Validation error messsages from the ERROR-STRING
     *          attributes of any record
     * Notes:   Performs no action, when there is no record with an ERROR-STRING in
     *          the ProDataset
     *          SCL-496
     * Throws:  Consultingwerk.Exceptions.DatasetException
     * @param phDataset The handle to the Dataset
     */
    METHOD PUBLIC STATIC VOID ThrowDatasetErrors (phDataset AS HANDLE):

        DEFINE VARIABLE cMessages AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE oError    AS DatasetException NO-UNDO .
        DEFINE VARIABLE i         AS INTEGER          NO-UNDO .

        cMessages = DatasetHelper:DatasetErrorStrings (phDataset) .

        IF cMessages > "":U THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 06.09.2011
               Split messages of ADM2 style error messages */
            ASSIGN oError = NEW DatasetException (ENTRY (1, cMessages, CHR(3)), 0) .

            DO i = 2 TO NUM-ENTRIES (cMessages, CHR(3)):
                oError:AddMessage (ENTRY (i, cMessages, CHR(3)), 0) .
            END.

            UNDO, THROW oError .
        END.

    END METHOD.

 END CLASS.
