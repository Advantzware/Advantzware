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
    File        : BufferHelper
    Purpose     :
    Syntax      : Static methods only, private constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 30 14:43:37 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Assertion.*             FROM PROPATH .
USING Consultingwerk.Exceptions.*            FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Consultingwerk.Util.Exceptions.*       FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Util.BufferHelper:

    /**
     * Purpose: Protected default constructor.
     * Notes:   There's no need to create instances of the helper classes
     */
    CONSTRUCTOR PROTECTED BufferHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Adds the fields from the given Buffer Handle to the given ListNameValuePair
     * Notes:   Raises an error, when no record is available in the Buffer Handle
     *          Does not add BLOB fields
     * @param phBuffer Then HANDLE of the Buffer that holds the record to log
     * @param poList The reference to an instance of the ListNameValuePair class
     * @param plPrefixWithBuffer Logical value indicating if field names should be prefixed with the table name
     */
    METHOD PUBLIC STATIC VOID AddBufferFieldsToList (phBuffer AS HANDLE,
                                                     poList AS Consultingwerk.ListNameValuePair,
                                                     plPrefixWithBuffer AS LOGICAL):

        DEFINE VARIABLE hField AS HANDLE  NO-UNDO .
        DEFINE VARIABLE i      AS INTEGER NO-UNDO .

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .

        fieldsLoop:
        DO i = 1 TO phBuffer:NUM-FIELDS:
            ASSIGN hField = phBuffer:BUFFER-FIELD (i) .

            IF hField:DATA-TYPE <> Consultingwerk.DataTypeEnum:BLOB THEN
                NEXT fieldsLoop .

            IF plPrefixWithBuffer THEN
                poList:Add (SUBSTITUTE ("&1.&2":U,
                                        phBuffer:NAME,
                                        hField:NAME),
                            STRING (hField:BUFFER-VALUE)) NO-ERROR .
            ELSE
                poList:Add (hField:NAME,
                            STRING (hField:BUFFER-VALUE)) NO-ERROR .
        END.

    END METHOD.

    /**
     * Purpose: Returns a comma-delimited list of the buffer column names
     * Notes:
     * @param phBuffer The buffer handle
     * @return The comma-delimited list of the buffer's column names
     */
    METHOD PUBLIC STATIC CHARACTER AllFieldNames (phBuffer AS HANDLE):

        DEFINE VARIABLE iField  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer ""BUFFER"":U} .

        DO iField = 1 TO phBuffer:NUM-FIELDS:
            ASSIGN cField = phBuffer:BUFFER-FIELD (iField):NAME
                   cFields = cFields + cField + ",":U .
        END.

        RETURN TRIM (cFields, ",":U) .

    END METHOD.

    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of all fields
     * @param phBuffer The handle of the buffer
     * @return The QueryString predicate
     */
    METHOD PUBLIC STATIC CHARACTER AllFieldsFindPredicate (phBuffer AS HANDLE):

        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.

        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW AppError ("The method AllFieldsFindPredicate needs to be executed using a valid buffer handle."{&TRAN}, 0) .

        IF NOT phBuffer:AVAILABLE THEN
            UNDO, THROW NEW AppError ("The method AllFieldsFindPredicate needs to be executed using a valid record."{&TRAN}, 0) .

        ASSIGN cQueryString = "WHERE ":U .

        DO iField = 1 TO phBuffer:NUM-FIELDS ON ERROR UNDO, THROW:
            ASSIGN hField = phBuffer:BUFFER-FIELD (iField)
                   cField = hField:NAME .

            IF iField > 1 THEN
                ASSIGN cQueryString = cQueryString + " AND ":U .

            ASSIGN cQueryString = cQueryString + phBuffer:NAME + ".":U + cField + " = ":U + QUOTER (hField:BUFFER-VALUE).
        END.

        RETURN cQueryString .

    END METHOD.

    /**
     * Purpose: Returns the result of a SUBSTITUTE function based on fields of a
     *          buffer handle
     * Notes:   Defaults to use the STRING-VALUE, not the BUFFER-VALUE
     * @param phBuffer The Buffer handle that contains the fields
     * @param pcFields The comma delimited list of the fields
     * @param pcSubstitute The pattern for the SUBSTITUTE function
     * @return The resulting string
     */
    METHOD PUBLIC STATIC CHARACTER BufferFieldSubstitute (phBuffer AS HANDLE,
                                                          pcFields AS CHARACTER,
                                                          pcSubstitute AS CHARACTER):

        RETURN BufferHelper:BufferFieldSubstitute (phBuffer,
                                                   pcFields,
                                                   pcSubstitute,
                                                   TRUE) .

    END METHOD .

    /**
     * Purpose: Returns the result of a SUBSTITUTE function based on fields of a
     *          buffer handle
     * Notes:   Does not support more than 9 entries
     * @param phBuffer The Buffer handle that contains the fields
     * @param pcFields The comma delimited list of the fields
     * @param pcSubstitute The pattern for the SUBSTITUTE function
     * @param plUseStringValue True to use the STRING-VALUE, false to use the BUFFER-VALUE
     * @return The resulting string
     */
    METHOD PUBLIC STATIC CHARACTER BufferFieldSubstitute (phBuffer AS HANDLE,
                                                          pcFields AS CHARACTER,
                                                          pcSubstitute AS CHARACTER,
                                                          plUseStringValue AS LOGICAL):

        DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO EXTENT 9.
        DEFINE VARIABLE iEntries AS INTEGER   NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO .

        ASSIGN iEntries = NUM-ENTRIES (pcFields) .

        IF iEntries > 9 THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcFields":U,
                                                            "Consultingwerk.Util.BufferHelper":U,
                                                            "Method BufferFieldSubstitute does not support more than 9 entries."{&TRAN},
                                                            0) .

        DO i = 1 TO iEntries:
            IF plUseStringValue THEN
                ASSIGN cValue [i] = phBuffer:BUFFER-FIELD (ENTRY (i, pcFields)):STRING-VALUE .
            ELSE DO:
                CASE phBuffer:BUFFER-FIELD (ENTRY (i, pcFields)):DATA-TYPE:
                    WHEN "ROWID":U THEN
                        ASSIGN cValue [i] = STRING (phBuffer:BUFFER-FIELD (ENTRY (i, pcFields)):BUFFER-VALUE) .
                    OTHERWISE
                        ASSIGN cValue [i] = phBuffer:BUFFER-FIELD (ENTRY (i, pcFields)):BUFFER-VALUE .
                END CASE .
            END.
        END.

        RETURN SUBSTITUTE (pcSubstitute,
                           cValue[1],
                           cValue[2],
                           cValue[3],
                           cValue[4],
                           cValue[5],
                           cValue[6],
                           cValue[7],
                           cValue[8],
                           cValue[9]) .
    END METHOD.

    /**
     * Purpose: Returns the result of a SUBSTITUTE function based on formatted fields of a
     *          buffer handle
     * Notes:   Does not support more than 9 entries
     * @param phBuffer The Buffer handle that contains the fields
     * @param pcFields The comma delimited list of the fields
     * @param pcSubstitute The pattern for the SUBSTITUTE function
     * @param pcFormats The formats to apply to the fields, delimited by the Tilde
     * @return The resulting string
     */
    METHOD PUBLIC STATIC CHARACTER BufferFieldSubstituteFormatted (phBuffer AS HANDLE,
                                                                   pcFields AS CHARACTER,
                                                                   pcSubstitute AS CHARACTER,
                                                                   pcFormats AS CHARACTER):

        DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO EXTENT 9.
        DEFINE VARIABLE cField   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cFormat  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iEntries AS INTEGER   NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO .

        ASSIGN iEntries = NUM-ENTRIES (pcFields) .

        IF iEntries > 9 THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcFields":U,
                                                            "Consultingwerk.Util.BufferHelper":U,
                                                            "Method BufferFieldSubstituteFormatted does not support more than 9 entries."{&TRAN},
                                                            0) .

        DO i = 1 TO NUM-ENTRIES (pcFields):
            ASSIGN cField = ENTRY (i, pcFields) .

            IF NUM-ENTRIES (pcFormats, CHR (126) /*"~~":U*/) >= i THEN
                cFormat = ENTRY (i, pcFormats, CHR (126) /*"~~":U*/) .
            ELSE cFormat = ? .

            DO ON ERROR UNDO, THROW:
                IF cFormat > "":U THEN
                    ASSIGN cValue [i] = TRIM(STRING (phBuffer:BUFFER-FIELD (cField):BUFFER-VALUE,
                                                     cFormat)) .
                ELSE
                    ASSIGN cValue [i] = phBuffer:BUFFER-FIELD (cField):STRING-VALUE .

                CATCH err AS Progress.Lang.Error:
                    /* ** Value 1234567890 cannot be displayed using >>>>9. (74) */
                    IF err:GetMessageNum (1) <> 74 THEN
                        UNDO, THROW err .

                    ASSIGN cValue [i] = BufferHelper:BufferFieldValue(phBuffer:BUFFER-FIELD (cField)) .
                END CATCH.
            END.
        END.

        RETURN SUBSTITUTE (pcSubstitute,
                           cValue[1],
                           cValue[2],
                           cValue[3],
                           cValue[4],
                           cValue[5],
                           cValue[6],
                           cValue[7],
                           cValue[8],
                           cValue[9]) .

    END METHOD.

    /**
     * Purpose: Returns a character representation of a buffer field value
     * Notes:   Tries to return the formatted STRING-VALUE. If the actual value
     *          is not complient with the fields format, it returns
     *          STRING(BUFFER-VALUE)
     * @param phField The handle of the buffer field
     * @return The character representation of the buffer field value
     */
    METHOD PUBLIC STATIC CHARACTER BufferFieldValue (phField AS HANDLE):

        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

        HandleAssert:WidgetType (phField, WidgetTypeEnum:BufferField) .
        BufferAssert:IsAvailable (phField:BUFFER-HANDLE) .

        ASSIGN cValue = phField:STRING-VALUE .

        RETURN cValue .

        CATCH err AS Progress.Lang.Error:
            /* ** Value 1234567890 cannot be displayed using >>>>9. (74) */
            IF err:GetMessageNum (1) <> 74 THEN
                UNDO, THROW err .

            RETURN STRING (phField:BUFFER-VALUE) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Returns the by clause mathing a buffer index
     * Notes:
     * @param phBuffer The handle of the buffer
     * @param pcIndexName The name of the index
     * @param plIncludeBuffer Include buffer name in by clause
     * @return The by clause mathing the index sort
     */
    METHOD PUBLIC STATIC CHARACTER BufferIndexByClause (phBuffer        AS HANDLE,
                                                        pcIndexName     AS CHARACTER,
                                                        plIncludeBuffer AS LOGICAL):

        RETURN BufferHelper:BufferIndexByClause (phBuffer, pcIndexName, plIncludeBuffer, FALSE) .

    END METHOD .

    /**
     * Purpose: Returns the by clause mathing a buffer index
     * Notes:
     * @param phBuffer The handle of the buffer
     * @param pcIndexName The name of the index
     * @param plIncludeBuffer Include buffer name in by clause
     * @param plReverseOrder Return reverse sort order (ASCENDING -> DESCENDING, DESCENDING -> ASCENDING)
     * @return The by clause mathing the index sort
     */
    METHOD PUBLIC STATIC CHARACTER BufferIndexByClause (phBuffer        AS HANDLE,
                                                        pcIndexName     AS CHARACTER,
                                                        plIncludeBuffer AS LOGICAL,
                                                        plReverseOrder  AS LOGICAL):

        DEFINE VARIABLE cSort  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cIndex AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iField AS INTEGER   NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .
        Assert:NotNull (plIncludeBuffer) .

        indexLoop:
        REPEAT ON ERROR UNDO, THROW:
            i = i + 1.

            cIndex = phBuffer:INDEX-INFORMATION (i) .

            IF cIndex > "":U THEN DO:

                IF ENTRY (1, cIndex) <> pcIndexName THEN
                    NEXT indexLoop .

                DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2:
                    IF plReverseOrder THEN
                        ASSIGN cSort = cSort + SUBSTITUTE (" BY &1&2&3":U,
                                                           (IF plIncludeBuffer THEN phBuffer:NAME + ".":U ELSE "":U),
                                                           ENTRY (iField, cIndex),
                                                           (IF ENTRY (iField + 1, cIndex) = "0":U THEN " DESCENDING":U ELSE "":U)) .
                    ELSE
                        ASSIGN cSort = cSort + SUBSTITUTE (" BY &1&2&3":U,
                                                           (IF plIncludeBuffer THEN phBuffer:NAME + ".":U ELSE "":U),
                                                           ENTRY (iField, cIndex),
                                                           (IF ENTRY (iField + 1, cIndex) = "1":U THEN " DESCENDING":U ELSE "":U)) .
                END.

                RETURN TRIM (cSort) .
            END.
            ELSE
                RETURN "":U .
        END.

    END METHOD .

    /**
     * Purpose: Returns the list of fields referenced in an index
     * Notes:
     * @param phBuffer The handle of the buffer
     * @param pcIndexName The name of the index
     * @return The comma delimited list of index fields
     */
    METHOD PUBLIC STATIC CHARACTER BufferIndexFields (phBuffer    AS HANDLE,
                                                      pcIndexName AS CHARACTER):

        DEFINE VARIABLE cFields AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cIndex  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iField  AS INTEGER   NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .

        indexLoop:
        REPEAT ON ERROR UNDO, THROW:
            i = i + 1.

            cIndex = phBuffer:INDEX-INFORMATION (i) .

            IF cIndex > "":U THEN DO:

                IF ENTRY (1, cIndex) <> pcIndexName THEN
                    NEXT indexLoop .

                DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2:

                    ASSIGN cFields = cFields + ENTRY (iField, cIndex) + ",":U .
                END.

                RETURN TRIM (cFields, ",":U) .
            END.
            ELSE
                RETURN "":U .
        END.

    END METHOD .

    /**
     * Purpose: Returns the large columns from the given buffer handle
     * Notes:   Used by QueryHelper:QueryLargeColumns
     * @param phBuffer The handle of the Buffer
     * @return The comma delimited list of large column names
     */
    METHOD PUBLIC STATIC CHARACTER BufferLargeColumns (phBuffer AS HANDLE):

        DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hField AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cLobs  AS CHARACTER NO-UNDO.

        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW AppError ("Invalid Buffer object."{&TRAN}, 0) .

        IF phBuffer:HAS-LOBS = FALSE THEN
            RETURN "":U .

        DO i = 1 TO phBuffer:NUM-FIELDS:
            ASSIGN hField = phBuffer:BUFFER-FIELD (i) .

            IF hField:DATA-TYPE = "BLOB":U OR hField:DATA-TYPE = "CLOB":U THEN
                ASSIGN cLobs = cLobs + hField:NAME + ",":U .
        END.

        RETURN TRIM (cLobs, ",":U) .

    END METHOD.

    /**
     * Purpose: Validates that a record can be created using the default values
     * Notes:   Checks if there is already a record matching the fields in the
     *          Unique Indexes
     * @param phBuffer The buffer handle
     * @return Logical value indicating if a record with default values can be created
     */
    METHOD PUBLIC STATIC LOGICAL CanCreateDefaultRow (phBuffer AS HANDLE):

        DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cIndex       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hFindBuffer  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.

        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW AppError ("The method CanCreateDefaultRow needs to be executed using a valid buffer handle."{&TRAN}, 0) .

        indexloop: REPEAT ON ERROR UNDO, THROW:
            ASSIGN iIndex = iIndex + 1
                   cIndex = phBuffer:INDEX-INFORMATION (iIndex) .

            IF cIndex > "":U THEN
                .
            ELSE
                LEAVE indexloop .

            IF ENTRY(2, cIndex) <> "1":U /* unique */ OR
               ENTRY(4, cIndex) <> "0":U /* word */ THEN
               NEXT indexloop .

            IF NOT VALID-HANDLE (hFindBuffer) THEN
                CREATE BUFFER hFindBuffer FOR TABLE phBuffer .

            ASSIGN cQueryString = "WHERE ":U .

            DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                ASSIGN cField = ENTRY(iField, cIndex)
                       hField = phBuffer:BUFFER-FIELD (cField) .

                /* When one of the unique's index fields default value is ?
                   then this index is not proplematic */
                IF hField:DEFAULT-VALUE = ? THEN
                    NEXT indexloop.

                IF iField > 5 THEN
                    ASSIGN cQueryString = cQueryString + " AND ":U .

                ASSIGN cQueryString = cQueryString + cField + " = ":U + QUOTER (hField:DEFAULT-VALUE).
            END.

            lFound = hFindBuffer:FIND-UNIQUE (cQueryString, NO-LOCK) NO-ERROR .

            IF lFound THEN
                RETURN FALSE .
        END.

        RETURN TRUE .

        FINALLY:
            IF VALID-HANDLE (hFindBuffer) THEN
                DELETE OBJECT hFindBuffer .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Concatenates the values of the given buffer fields
     * Notes:   Alternative to the BufferFieldSubstitute method when more
     *          than 9 field values are required
     * @param phBuffer The handle of the buffer
     * @param pcFields The comma delimited list of fields
     * @param pcDelimiter The delimiter to use
     * @return The concatenated field values
     */
    METHOD PUBLIC STATIC CHARACTER ConcatenateFieldValues (phBuffer AS HANDLE,
                                                           pcFields AS CHARACTER,
                                                           pcDelimiter AS CHARACTER):

        DEFINE VARIABLE cList  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO .

        ASSIGN cList = FILL (pcDelimiter, NUM-ENTRIES (pcFields) - 1) .

        DO i = 1 TO NUM-ENTRIES (pcFields):

            CASE phBuffer:BUFFER-FIELD (ENTRY (i, pcFields)):DATA-TYPE:
                WHEN "ROWID":U THEN
                    ASSIGN cValue = STRING (phBuffer:BUFFER-FIELD (ENTRY (i, pcFields)):BUFFER-VALUE) .
                OTHERWISE
                    ASSIGN cValue = phBuffer:BUFFER-FIELD (ENTRY (i, pcFields)):BUFFER-VALUE .
            END CASE .

            IF cValue = ? THEN
                ASSIGN cValue = "?":U .

            ENTRY (i, cList, pcDelimiter) = cValue .
        END.

        RETURN cList .

    END METHOD.

    /**
     * Purpose: Returns the handle to the database buffer referenced by the
     *          BufferFieldName object
     * Notes:
     * @param poBufferFieldName The reference to the BufferFieldName object
     * @return The handle to the buffer references by the BufferFieldName object
     */
    METHOD PUBLIC STATIC HANDLE BufferHandle (poBufferFieldName AS BufferFieldName):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        IF poBufferFieldName:DatabaseName > "":U AND poBufferFieldName:TableName > "":U THEN
            CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE ("&1.&2":U,
                                                        poBufferFieldName:DatabaseName,
                                                        poBufferFieldName:TableName) .
        ELSE
            CREATE BUFFER hBuffer FOR TABLE poBufferFieldName:TableName .

        RETURN hBuffer.

    END METHOD .

    /**
     * Purpose: Creates and returns a dynamic buffer based on a BufferFieldName object
     * Notes:   The caller is expected to delete the buffer object handle when it's
     *          no longer needed
     * @param poBufferFieldName The BufferFieldName reference
     * @return The handle of the dynamic buffer
     */
    METHOD PUBLIC STATIC HANDLE CreateBuffer (poBufferFieldName AS BufferFieldName):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        IF poBufferFieldName:DatabaseName > "":U THEN
            CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE ("&1.&2":U,
                                                        poBufferFieldName:DatabaseName,
                                                        poBufferFieldName:TableName) .
        ELSE
            CREATE BUFFER hBuffer FOR TABLE poBufferFieldName:TableName .

        RETURN hBuffer.

    END METHOD .

    /**
     * Purpose: Creates a Dynamic buffer on the fly
     * Notes:   Useful to pass a new buffer handle without the need to define a
     *          variable upfront (e.g. ProDataset:ADD-BUFFER, etc.)
     * @param phTable The handle of the table or existing buffer which can specify the source for which to create the buffer object
     * @return The handle of the new dynamic buffer
     */
    METHOD PUBLIC STATIC HANDLE CreateBuffer (phTable AS HANDLE):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phTable} .

        CREATE BUFFER hBuffer FOR TABLE phTable .

        RETURN hBuffer .

    END METHOD.

    /**
     * Purpose: Returns a buffer fields SERIALIZE-NAME when set,
     *          otherwise the fields NAME
     * Notes:
     * @param phField The handle of a temp-table field
     * @return The fields SERIALIZE-NAME or NAME
     */
    METHOD PUBLIC STATIC CHARACTER FieldSerializeName (phField AS HANDLE):

        HandleAssert:WidgetType (phField, WidgetTypeEnum:BufferField) .
        BufferAssert:IsTempTableBuffer (phField:BUFFER-HANDLE) .

        IF phField:SERIALIZE-NAME > "":U THEN
            RETURN phField:SERIALIZE-NAME .

        RETURN phField:NAME .

    END METHOD.

    /**
     * Purpose: Finds the before buffer's after-buffer record
     * Notes:
     * @param phBeforeBuffer The handle of the before buffer
     */
    METHOD PUBLIC STATIC VOID FindAfterBuffer (phBeforeBuffer AS HANDLE):

        BufferAssert:IsDatasetBuffer (phBeforeBuffer) .

        phBeforeBuffer:AFTER-BUFFER:FIND-BY-ROWID (phBeforeBuffer:AFTER-ROWID) .

    END METHOD .

    /**
     * Purpose: Finds the after buffer's before-buffer record
     * Notes:
     * @param phAfterBuffer The handle of the after buffer
     */
    METHOD PUBLIC STATIC VOID FindBeforeBuffer (phAfterBuffer AS HANDLE):

        BufferAssert:IsDatasetBuffer (phAfterBuffer) .

        phAfterBuffer:BEFORE-BUFFER:FIND-BY-ROWID (phAfterBuffer:BEFORE-ROWID) .

    END METHOD .

    /**
     * Purpose: Finds the database record for a linked (ProDataset data-source) record
     *          in a ProDataset.
     * Notes:   Uses the PUK or first UK of the database record with the field values
     *          of the ProDataset buffer, based on the provided field mapping
     * @param phBuffer The database buffer to locate
     * @param phDatasetBuffer The dataset (before or after) buffer
     * @param pcFieldMapping The field mapping between database and temp-table (ATTACH-DATA-SOURCE method)
     */
    METHOD PUBLIC STATIC VOID FindDatabaseBuffer (phBuffer AS HANDLE,
                                                  phDatasetBuffer AS HANDLE,
                                                  pcFieldMapping AS CHARACTER):

        DEFINE VARIABLE cKeyFields   AS CHARACTER NO-UNDO.

        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cField       AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hField       AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iMapping     AS INTEGER   NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDatasetBuffer WidgetTypeEnum:Buffer} .
        BufferAssert:IsDatasetBuffer (phDatasetBuffer) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phDatasetBuffer} .

        ASSIGN cKeyFields = BufferHelper:UniqueKeyFields (phBuffer, TRUE, TRUE) .

        IF cKeyFields > "":U THEN .
        ELSE
            ASSIGN cKeyFields = BufferHelper:UniqueKeyFields (phBuffer, FALSE, TRUE) .

        IF cKeyFields > "":U THEN .
        ELSE
            UNDO, THROW NEW UniqueIndexRequiredException (SUBSTITUTE ("Unable to perform FindDatabaseBuffer for buffer &1 when there is no unique index!"{&TRAN}, phBuffer:NAME),
                                                          0,
                                                          phBuffer:NAME) .

        DO iField = 1 TO NUM-ENTRIES (cKeyFields):

            ASSIGN cField = ENTRY (iField, cKeyFields) .

            ASSIGN iMapping = LOOKUP(cField, pcFieldMapping) .

            IF iMapping > 1 THEN
                ASSIGN hField = phDatasetBuffer:BUFFER-FIELD (BufferHelper:ParseFieldName (ENTRY (iMapping - 1, pcFieldMapping)):FieldName) .
            ELSE
                ASSIGN hField = phDatasetBuffer:BUFFER-FIELD (BufferHelper:ParseFieldName (cField):FieldName) .

           ASSIGN cQueryString = cQueryString + SUBSTITUTE (" &1 &2 = &3":U,
                                                            (IF iField = 1 THEN "WHERE":U ELSE "AND":U),
                                                            cField,
                                                            QUOTER (hField:BUFFER-VALUE)) .
        END.

        phBuffer:FIND-UNIQUE (cQueryString) . /* throw on error */

    END METHOD.

    /**
     * Purpose: Returns the name and extent of the field name
     * Notes:
     * @param pcField The field name, including extent if present
     * @param pcFieldName OUTPUT Just the field name portion
     * @param piExtent OUTPUT The field extent or 0 when the field name did not contain extent information
     */
    METHOD PUBLIC STATIC VOID GetFieldNameAndExtent (pcField AS CHARACTER,
                                                     OUTPUT pcFieldName AS CHARACTER,
                                                     OUTPUT piExtent AS INTEGER):

        DEFINE VARIABLE cExtent AS CHARACTER   NO-UNDO.

        IF INDEX (pcField, "[":U) > 0 THEN
            ASSIGN pcFieldName = SUBSTRING (pcField, 1, INDEX (pcField, "[":U) - 1, "CHARACTER":U)
                   cExtent     = SUBSTRING (pcField, INDEX (pcField, "[":U) + 1, -1, "CHARACTER":U)
                   piExtent    = INTEGER (SUBSTRING(cExtent, 1, LENGTH (cExtent, "CHARACTER":U) - 1, "CHARACTER":U))
                        .
        ELSE
            ASSIGN pcFieldName = pcField
                   piExtent    = 0 .

    END METHOD.

    /**
     * Purpose: Returns a dictionary with field values of the given buffer
     * Notes:
     * @param phBuffer The handle of the buffer
     * @param pcFieldNames The comma delimited list of field names to return the values for
     * @return The CharcaterDictionary with the field values
     */
    METHOD PUBLIC STATIC CharacterDictionary GetFieldValues (phBuffer AS HANDLE,
                                                             pcFieldNames AS CHARACTER):

        DEFINE VARIABLE oReturn AS CharacterDictionary NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cField  AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oField  AS BufferFieldName     NO-UNDO .

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcFieldNames} .

        oReturn = NEW CharacterDictionary ("":U, ",":U, "":U, CHR(1)) .

        DO i = 1 TO NUM-ENTRIES (pcFieldNames):
            ASSIGN cField = ENTRY (i, pcFieldNames)
                   oField = BufferHelper:ParseFieldName (cField) .

            BufferAssert:HasField (phBuffer, oField:FieldName) .

            IF oField:Index > 0 THEN
                oReturn:Add (cField, STRING(phBuffer:BUFFER-FIELD (oField:FieldName):BUFFER-VALUE [oField:Index])) .
            ELSE
                oReturn:Add (cField, STRING(phBuffer:BUFFER-FIELD (oField:FieldName):BUFFER-VALUE)) .
        END.

        RETURN oReturn .

    END METHOD .

    /**
     * Purpose: Tests if the passed in buffer handle has a field with the
     *          given name
     * Notes:   Verifies that a valid buffer is passed in first.
     * @param phBuffer The Buffer handle to test
     * @param pcFieldName The name of the buffer field
     * @return Logical value indicating if the field exists
     */
    METHOD PUBLIC STATIC LOGICAL HasField (phBuffer AS HANDLE,
                                           pcFieldName AS CHARACTER):

        DEFINE VARIABLE hField AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phBuffer """BUFFER"":U"} .

        hField = phBuffer:BUFFER-FIELD (pcFieldName) NO-ERROR .

        RETURN VALID-HANDLE (hField) .

        FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Returns TRUE when the passed in Buffer Handle is a buffer handle for
     *          a database table
     * Notes:
     * @param phBuffer The buffer handle to test
     * @return Logical value indicating if the buffer handle is for a database buffer
     */
    METHOD PUBLIC STATIC LOGICAL IsDatabaseBuffer (phBuffer AS HANDLE):

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .

        IF CharacterType:IsNullOrEmpty (phBuffer:DBNAME) OR
           CharacterType:IsNullOrEmpty (DBTYPE (phBuffer:DBNAME)) THEN

            RETURN FALSE .
        ELSE
            RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Returns if the provided list of field names is unqiue
     * Notes:   Tests if there is an unique index defined for the buffer
     *          where all index fields are contained in the list of fields
     *          Base for the DatasetHelper:IsUniqueChildRelation method
     * @param phBuffer The handle of the Buffer (database or temp-table) to test for
     * @param pcFields The comma delimited list of fields to test for
     * @return Logical value indicating if the provided field combination satisfies a unique index
     */
    METHOD PUBLIC STATIC LOGICAL IsUniqueFieldCombination (phBuffer AS HANDLE,
                                                           pcFields AS CHARACTER):

        DEFINE VARIABLE iIndex AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iField AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cIndex AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cField AS CHARACTER NO-UNDO .

        HandleAssert:WidgetType(phBuffer, WidgetTypeEnum:Buffer) .

        indexloop: REPEAT ON ERROR UNDO, THROW:
            ASSIGN iIndex = iIndex + 1
                   cIndex = phBuffer:INDEX-INFORMATION (iIndex) .

            IF cIndex > "":U THEN .
            ELSE
                LEAVE indexloop .

            IF ENTRY(2, cIndex) <> "1":U /* unique */ OR
               ENTRY(4, cIndex) =  "1":U /* word */ THEN
               NEXT indexloop  .

            DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                ASSIGN cField = ENTRY(iField, cIndex).

                /* First Index Field not in list, invalidates this index */
                IF NOT ListHelper:EntryIsInList(cField, pcFields) THEN
                    NEXT indexloop .
            END.

            RETURN TRUE .
        END.

        RETURN FALSE.

    END METHOD.

    /**
     * Purpose: Assigns a new GUID value to a buffer field when the buffer field is
     *          empty or ?
     * Notes:   Useful in a Data Access class, to conditionally assign a GUID when
     *          the client might or might not have done so yet
     * @param phBuffer The Buffer handle
     * @param pcFieldName The name of the buffer field (type CHARACTER)
     * @return The GUID value (either existing in the buffer field or the new value)
     */
    METHOD PUBLIC STATIC CHARACTER NewGuidWhenEmpty (phBuffer AS HANDLE,
                                                     pcFieldName AS CHARACTER):

        DEFINE VARIABLE cGuid AS CHARACTER NO-UNDO.

        BufferAssert:HasField (phBuffer, pcFieldName) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .

        ASSIGN cGuid = phBuffer:BUFFER-FIELD (pcFieldName):BUFFER-VALUE .

        IF cGuid = ? OR cGuid = "":U THEN DO:
            ASSIGN cGuid                                            = GUID
                   phBuffer:BUFFER-FIELD (pcFieldName):BUFFER-VALUE = cGuid .
        END.

        RETURN cGuid .

    END METHOD .

    /**
     * Purpose: Nullify (?) all unique index field values
     * Notes:   This methods can be used when creating records, but there is already
     *          a record in the table, that uses the default values of some index fields
     * @param phBuffer The buffer handle
     */
    METHOD PUBLIC STATIC VOID NullifyUniqueIndexFields (phBuffer AS HANDLE):

        DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cIndex       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField       AS HANDLE    NO-UNDO.

        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW AppError ("The method NullifyUniqueIndexFields needs to be executed using a valid buffer handle."{&TRAN}, 0) .

        indexloop: REPEAT ON ERROR UNDO, THROW:
            ASSIGN iIndex = iIndex + 1
                   cIndex = phBuffer:INDEX-INFORMATION (iIndex) .

            IF cIndex > "":U THEN .
            ELSE
                LEAVE indexloop .

            IF ENTRY(2, cIndex) <> "1":U /* unique */ OR
               ENTRY(4, cIndex) <> "0":U /* word */ THEN
               NEXT indexloop  .

            DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                ASSIGN cField = ENTRY(iField, cIndex)
                       hField = phBuffer:BUFFER-FIELD (cField)

                       hField:BUFFER-VALUE = ? .
            END.
        END.

    END METHOD.

    /**
     * Purpose: Returns the number of records available in the Buffer
     * Notes:
     * @param phBuffer The buffer handle
     * @return The number of records available in the buffer
     */
    METHOD PUBLIC STATIC INTEGER NumRecords (phBuffer AS HANDLE):

        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .

        hQuery = QueryHelper:CreatePreparedQuery (phBuffer,
                                                  SUBSTITUTE ("PRESELECT EACH &1":U, phBuffer:NAME)) .

        RETURN hQuery:NUM-RESULTS .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Assigns buffer field values from SAX-ATTRIBUTES
     * Notes:
     * @param phBuffer The handle to the buffer to assign the field values to
     * @param phSaxAttributes The handle of the SAX-ATTRIBUTES widget
     */
    METHOD PUBLIC STATIC VOID ParseSaxAttributesToBuffer (phBuffer AS HANDLE,
                                                          phSaxAttributes AS HANDLE):

        DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cName  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phSaxAttributes WidgetTypeEnum:SaxAttributes} .

        DO i = 1 TO phSaxAttributes:NUM-ITEMS:
            ASSIGN cName  = phSaxAttributes:GET-QNAME-BY-INDEX (i)
                   cValue = phSaxAttributes:GET-VALUE-BY-INDEX (i) .

            Consultingwerk.Assertion.BufferAssert:HasField (phBuffer, cName) .

            ASSIGN phBuffer:BUFFER-FIELD (cName):BUFFER-VALUE = cValue .
        END.

    END METHOD .

    /**
     * Purpose: Parses a field name
     * Notes:   Supports db.table.field, table.field, field with and without index (extent)
     * @param pcFieldName The field name to parse
     * @return The Consultingwerk.BufferFieldName struct describing the field name
     */
    METHOD PUBLIC STATIC BufferFieldName ParseFieldName (pcFieldName AS CHARACTER):

        RETURN NEW BufferFieldName (pcFieldName) .

    END METHOD .

    /**
     * Purpose: Returns the qualified table name of a buffer
     * Notes:   For a DB buffer the qualified table name is prefixed with the database name
     *          For a temp-table buffer the qualified table name is the same as the temp-table name
     * @param phBuffer The handle of the buffer
     * @return The qualified table name
     */
    METHOD PUBLIC STATIC CHARACTER QualifiedTableName (phBuffer AS HANDLE):

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer ""BUFFER"":U} .

        IF BufferHelper:IsDatabaseBuffer (phBuffer) THEN
            RETURN SUBSTITUTE ("&1.&2":U,
                               phBuffer:DBNAME,
                               phBuffer:TABLE) .
        ELSE
            RETURN phBuffer:TABLE .

    END METHOD .

    /**
     * Purpose: Shows buffer field values on the screen using the MESSAGE statement
     * Notes:   Defaults to show the first 15 fields, throws error on invalid buffer handle
     * @param phBuffer The handle of the Buffer to output
     */
    METHOD PUBLIC STATIC VOID ShowBuffer (phBuffer AS HANDLE):

        BufferHelper:ShowBuffer (phBuffer, 15, TRUE, "":U) .

    END METHOD .

    /**
     * Purpose: Shows buffer field values on the screen using the MESSAGE statement
     * Notes:   Defaults to show the first 15 fields, throws error on invalid buffer handle
     * @param phBuffer The handle of the Buffer to output
     * @param pcMessageText An additional message text to show before the buffer contents
     */
    METHOD PUBLIC STATIC VOID ShowBuffer (phBuffer AS HANDLE,
                                          pcMessageText AS CHARACTER):

        BufferHelper:ShowBuffer (phBuffer, 15, TRUE, pcMessageText) .

    END METHOD .

    /**
     * Purpose: Shows buffer field values on the screen using the MESSAGE statement
     * Notes:   Defaults to show the first 15 fields
     * @param phBuffer The handle of the Buffer to output
     * @param plThrowOnInvalidHandle Logical value indicating if the method should throw an error on an invalid widget handle
     */
    METHOD PUBLIC STATIC VOID ShowBuffer (phBuffer AS HANDLE,
                                          plThrowOnInvalidHandle AS LOGICAL):

        BufferHelper:ShowBuffer (phBuffer, 15, plThrowOnInvalidHandle, "":U) .

    END METHOD .

    /**
     * Purpose: Shows buffer field values on the screen using the MESSAGE statement
     * Notes:   Defaults to show the first 15 fields
     * @param phBuffer The handle of the Buffer to output
     * @param plThrowOnInvalidHandle Logical value indicating if the method should throw an error on an invalid widget handle
     * @param pcMessageText An additional message text to show before the buffer contents
     */
    METHOD PUBLIC STATIC VOID ShowBuffer (phBuffer AS HANDLE,
                                          plThrowOnInvalidHandle AS LOGICAL,
                                          pcMessageText AS CHARACTER):

        BufferHelper:ShowBuffer (phBuffer, 15, plThrowOnInvalidHandle, pcMessageText) .

    END METHOD .

    /**
     * Purpose: Shows buffer field values on the screen using the MESSAGE statement
     * Notes:   When number of fields is 0 all fields will be shown, throws error on invalid buffer handle
     * @param phBuffer The handle of the Buffer to output
     * @param piNumFields The maximum number of fields to show
     */
    METHOD PUBLIC STATIC VOID ShowBuffer (phBuffer AS HANDLE,
                                          piNumFields AS INTEGER):

        BufferHelper:ShowBuffer (phBuffer, piNumFields, TRUE, "":U) .

    END METHOD .

    /**
     * Purpose: Shows buffer field values on the screen using the MESSAGE statement
     * Notes:   When number of fields is 0 all fields will be shown, throws error on invalid buffer handle
     * @param phBuffer The handle of the Buffer to output
     * @param piNumFields The maximum number of fields to show
     * @param pcMessageText An additional message text to show before the buffer contents
     */
    METHOD PUBLIC STATIC VOID ShowBuffer (phBuffer AS HANDLE,
                                          piNumFields AS INTEGER,
                                          pcMessageText AS CHARACTER):

        BufferHelper:ShowBuffer (phBuffer, piNumFields, TRUE, pcMessageText) .

    END METHOD .

    /**
     * Purpose: Shows buffer field values on the screen using the MESSAGE statement
     * Notes:   When number of fields is 0 all fields will be shown
     * @param phBuffer The handle of the Buffer to output
     * @param piNumFields The maximum number of fields to show
     * @param plThrowOnInvalidHandle Logical value indicating if the method should throw an error on an invalid widget handle
     */
    METHOD PUBLIC STATIC VOID ShowBuffer (phBuffer AS HANDLE,
                                          piNumFields AS INTEGER,
                                          plThrowOnInvalidHandle AS LOGICAL):

        BufferHelper:ShowBuffer (phBuffer, piNumFields, plThrowOnInvalidHandle, "":U) .

    END METHOD .

    /**
     * Purpose: Shows buffer field values on the screen using the MESSAGE statement
     * Notes:   When number of fields is 0 all fields will be shown
     * @param phBuffer The handle of the Buffer to output
     * @param piNumFields The maximum number of fields to show
     * @param plThrowOnInvalidHandle Logical value indicating if the method should throw an error on an invalid widget handle
     * @param pcMessageText An additional message text to show before the buffer contents
     */
    METHOD PUBLIC STATIC VOID ShowBuffer (phBuffer AS HANDLE,
                                          piNumFields AS INTEGER,
                                          plThrowOnInvalidHandle AS LOGICAL,
                                          pcMessageText AS CHARACTER):

        DEFINE VARIABLE iField  AS INTEGER   NO-UNDO INITIAL 1.
        DEFINE VARIABLE iExtent AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hField  AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cValue  AS CHARACTER NO-UNDO .

        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO .

        IF plThrowOnInvalidHandle THEN DO:
            {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .
        END.
        ELSE DO:
            IF NOT VALID-HANDLE (phBuffer) THEN DO:
                MESSAGE (IF pcMessageText > "":U THEN SUBSTITUTE ("&1~n~n":U, pcMessageText) ELSE "":U)
                        "<Invalid Buffer Handle>"{&TRAN}
                    VIEW-AS ALERT-BOX INFORMATION .

                RETURN .
            END.

            IF NOT phBuffer:TYPE = "BUFFER":U THEN DO:
                MESSAGE (IF pcMessageText > "":U THEN SUBSTITUTE ("&1~n~n":U, pcMessageText) ELSE "":U)
                        "<Not a Buffer Handle>"{&TRAN}
                    VIEW-AS ALERT-BOX INFORMATION .

                RETURN .
            END.
        END.

        IF NOT phBuffer:AVAILABLE THEN DO:
            MESSAGE (IF pcMessageText > "":U THEN SUBSTITUTE ("&1~n~n":U, pcMessageText) ELSE "":U)
                    SUBSTITUTE ("Buffer &1: <no record is available>"{&TRAN},
                                phBuffer:NAME) VIEW-AS ALERT-BOX INFORMATION .

            RETURN .
        END.

        ASSIGN cMessage = (IF pcMessageText > "":U THEN SUBSTITUTE ("&1~n~n":U, pcMessageText) ELSE "":U) +
                          SUBSTITUTE ("Buffer &1: record with ROWID &2~n~n"{&TRAN},
                                      phBuffer:NAME,
                                      phBuffer:ROWID) .

        fieldsLoop:
        REPEAT ON ERROR UNDO, THROW:
            ASSIGN hField = phBuffer:BUFFER-FIELD (iField) .

            IF hField:EXTENT >= 1 THEN DO:
                DO iExtent = 1 TO hField:EXTENT:
                    CASE hField:DATA-TYPE:
                        WHEN "BLOB":U THEN
                            ASSIGN cValue = SUBSTITUTE ("Length: &1"{&TRAN}, LENGTH (hField:BUFFER-VALUE(iExtent), "RAW":U)) .

                        WHEN "CLOB":U THEN
                            ASSIGN cValue = SUBSTITUTE ("Length: &1"{&TRAN}, LENGTH (hField:BUFFER-VALUE, "CHARACTER":U)) .

                        OTHERWISE
                            ASSIGN cValue = STRING (hField:BUFFER-VALUE(iExtent)) .

                    END CASE .

                    cMessage = cMessage + SUBSTITUTE ("&1.&2[&5] <&3>:    &4~n":U,
                                                      phBuffer:NAME,
                                                      hField:NAME,
                                                      hField:DATA-TYPE,
                                                      cValue,
                                                      iExtent) .
                END.
            END.
            ELSE DO:
                CASE hField:DATA-TYPE:
                    WHEN "BLOB":U THEN
                        ASSIGN cValue = SUBSTITUTE ("Length: &1"{&TRAN}, LENGTH (hField:BUFFER-VALUE, "RAW":U)) .

                    WHEN "CLOB":U THEN
                        ASSIGN cValue = SUBSTITUTE ("Length: &1"{&TRAN}, LENGTH (hField:BUFFER-VALUE, "CHARACTER":U)) .

                    OTHERWISE
                        ASSIGN cValue = STRING (hField:BUFFER-VALUE) .

                END CASE .

                cMessage = cMessage + SUBSTITUTE ("&1.&2 <&3>:    &4~n":U,
                                                  phBuffer:NAME,
                                                  hField:NAME,
                                                  hField:DATA-TYPE,
                                                  cValue) .
            END.

            ASSIGN iField = iField + 1 .

            IF iField > phBuffer:NUM-FIELDS THEN
                LEAVE fieldsLoop .

            IF piNumFields > 0 AND iField > piNumFields THEN
                LEAVE fieldsLoop .
        END.

        MESSAGE cMessage
            VIEW-AS ALERT-BOX INFORMATION .

    END METHOD .

    /**
     * Purpose: Returns the DATA-SOURCE field mapping for the given source buffer
     * Notes:   Filters the buffer's DATA-SOURCE-COMPLETE-MAP by the source buffer
     *          name entries
     * @param phBuffer The ProDataset temp-table buffer
     * @param pcSourceBufferName The name of the source buffer (typically database buffer)
     * @return CHARACTER
     */
    METHOD PUBLIC STATIC CHARACTER SourceBufferMapping (phBuffer AS HANDLE,
                                                        pcSourceBufferName AS CHARACTER):

        DEFINE VARIABLE cMapping AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO .

        BufferAssert:IsDatasetBuffer (phBuffer) .

        DO i = 2 TO NUM-ENTRIES (phBuffer:DATA-SOURCE-COMPLETE-MAP) BY 2:

            IF ENTRY (i, phBuffer:DATA-SOURCE-COMPLETE-MAP) BEGINS SUBSTITUTE ("&1.":U, pcSourceBufferName) THEN
                ASSIGN cMapping = cMapping + SUBSTITUTE ("&1,&2,":U,
                                                         ENTRY (i - 1, phBuffer:DATA-SOURCE-COMPLETE-MAP),
                                                         ENTRY (i, phBuffer:DATA-SOURCE-COMPLETE-MAP)) .
        END.

        ASSIGN cMapping = TRIM (cMapping, ",":U) .

        RETURN cMapping .

    END METHOD.

    /**
     * Purpose: Returns the total number of fields in a Buffer including Array
     *          fields as individual columns
     * Notes:
     * @param phBuffer The handle to the buffer
     * @return The total number of columns, including array extents
     */
    METHOD PUBLIC STATIC INTEGER TotalNumberOfFields (phBuffer AS HANDLE):

        DEFINE VARIABLE i      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iTotal AS INTEGER NO-UNDO.
        DEFINE VARIABLE hField AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer ""BUFFER"":U} .

        DO i = 1 TO phBuffer:NUM-FIELDS:

            ASSIGN hField = phBuffer:BUFFER-FIELD (i) .

            IF hField:EXTENT = 0 THEN
                ASSIGN iTotal = iTotal + 1 .
            ELSE
                ASSIGN iTotal = iTotal + phBuffer:EXTENT .
        END.

        RETURN iTotal .

    END METHOD.

    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of primary unique columns
     * Notes:   Returns "" when there is no primary unique index.
     * @param phBuffer The handle of the buffer
     * @param plIncludeBufferName Logical value indicating if the buffer name should be included in the field names
     * @return The query string predicate
     */
    METHOD PUBLIC STATIC CHARACTER UniqueFindPredicate (phBuffer AS HANDLE,
                                                        plIncludeBufferName AS LOGICAL):

        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW AppError ("The method UniqueFindPredicate needs to be executed using a valid buffer handle."{&TRAN}, 0) .

        IF plIncludeBufferName = TRUE THEN
            RETURN BufferHelper:UniqueFindPredicateWithValueBuffer (phBuffer,
                                                                    phBuffer,
                                                                    phBuffer:NAME) .
        ELSE
            RETURN BufferHelper:UniqueFindPredicateWithValueBuffer (phBuffer,
                                                                    phBuffer,
                                                                    "":U) .

    END METHOD .

    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of primary unique columns
     * Notes:   Returns "" when there is no primary unique index.
     * @param phBuffer The handle of the buffer
     * @param pcBufferName The Buffer name to prefix fields with, leave empty to not apply the fields with a buffer name
     * @return The query string predicate
     */
    METHOD PUBLIC STATIC CHARACTER UniqueFindPredicate (phBuffer AS HANDLE,
                                                        pcBufferName AS CHARACTER):

        RETURN BufferHelper:UniqueFindPredicateWithValueBuffer (phBuffer,
                                                                phBuffer,
                                                                pcBufferName) .

    END METHOD .

    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of primary unique columns
     * Notes:   Returns "" when there is no primary unique index.
     * @param phBuffer The handle of the buffer
     * @param phValueBuffer The buffer to access field values from
     * @param pcBufferName The Buffer name to prefix fields with, leave empty to not apply the fields with a buffer name
     * @return The query string predicate
     */
    METHOD PUBLIC STATIC CHARACTER UniqueFindPredicateWithValueBuffer (phBuffer AS HANDLE,
                                                                       phValueBuffer AS HANDLE,
                                                                       pcBufferName AS CHARACTER):

        RETURN BufferHelper:UniqueFindPredicateWithValueBuffer (phBuffer,
                                                                phValueBuffer,
                                                                ?,
                                                                pcBufferName) .

    END METHOD .

    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of primary unique columns
     * Notes:   Returns "" when there is no primary unique index.
     * @param phBuffer The handle of the buffer
     * @param phValueBuffer The buffer to access field values from
     * @param poMissingValues The dictionary of character values for index fields from the phBuffer not present in phValueBuffer
     * @param pcBufferName The Buffer name to prefix fields with, leave empty to not apply the fields with a buffer name
     * @return The query string predicate
     */
    METHOD PUBLIC STATIC CHARACTER UniqueFindPredicateWithValueBuffer (phBuffer AS HANDLE,
                                                                       phValueBuffer AS HANDLE,
                                                                       poMissingValues AS CharacterDictionary,
                                                                       pcBufferName AS CHARACTER):

        DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cIndex       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hIndexBuffer AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cValue       AS CHARACTER NO-UNDO.

        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW AppError ("The method UniqueFindPredicate(WithValueBuffer) needs to be executed using a valid buffer handle."{&TRAN}, 0) .

        IF NOT VALID-HANDLE (phValueBuffer) OR phValueBuffer:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW AppError ("The method UniqueFindPredicate(WithValueBuffer) needs to be executed using a valid buffer handle."{&TRAN}, 0) .

        IF NOT phValueBuffer:AVAILABLE THEN
            UNDO, THROW NEW AppError ("The method UniqueFindPredicate(WithValueBuffer) needs to be executed using a valid record."{&TRAN}, 0) .

        /* Mike Fechner, Consultingwerk Ltd. 26.11.2010
           When we are working on the BEFORE-BUFFER, the BEFORE-BUFFER has no
           indexes, then use the AFTER-BUFFER for the indexes */
        IF VALID-HANDLE (phBuffer:AFTER-BUFFER) THEN hIndexBuffer = phBuffer:AFTER-BUFFER .
        ELSE hIndexBuffer = phBuffer .

        indexloop: REPEAT ON ERROR UNDO, THROW:
            ASSIGN iIndex = iIndex + 1
                   cIndex = hIndexBuffer:INDEX-INFORMATION (iIndex) .

            IF cIndex > "":U THEN .
            ELSE
                LEAVE indexloop .

            IF ENTRY(2, cIndex) <> "1":U /* unique */ OR
               ENTRY(3, cIndex) <> "1":U /* primary */ THEN
               NEXT indexloop .

            ASSIGN cQueryString = "WHERE ":U .

            DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                ASSIGN cField = ENTRY(iField, cIndex) .

                IF BufferHelper:HasField (phValueBuffer, cField) THEN
                    ASSIGN hField = phValueBuffer:BUFFER-FIELD (cField)
                           cValue = QUOTER (hField:BUFFER-VALUE) .
                ELSE IF VALID-OBJECT (poMissingValues) AND poMissingValues:ContainsKey (cField) THEN
                    ASSIGN cValue = QUOTER (poMissingValues:GetValue (cField)) .
                ELSE
                    UNDO, THROW NEW MissingIndexFieldValueException ("Unable to build unique find predicate. Value for index field &1.&2 is not provided."{&TRAN},
                                                                     phBuffer:NAME,
                                                                     cField) .

                IF iField > 5 THEN
                    ASSIGN cQueryString = cQueryString + " AND ":U .

                ASSIGN cQueryString = cQueryString +
                                      (IF pcBufferName > "":U THEN pcBufferName + ".":U ELSE "":U) +
                                      cField + " = ":U + cValue .
            END.

            LEAVE indexloop .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 08.01.2012
           If QueryString is empty here, the table does not
           have a primary unique index, so try the first unique
           index */
        IF cQueryString = "":U THEN DO:
            iIndex = 0 .

            indexloop: REPEAT ON ERROR UNDO, THROW:
                ASSIGN iIndex = iIndex + 1
                       cIndex = hIndexBuffer:INDEX-INFORMATION (iIndex) .

                IF cIndex > "":U THEN .
                ELSE
                    LEAVE indexloop .

                IF ENTRY(2, cIndex) <> "1":U /* unique */ THEN
                   NEXT indexloop .

                ASSIGN cQueryString = "WHERE ":U .

                DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                    ASSIGN cField = ENTRY(iField, cIndex) .

                IF BufferHelper:HasField (phValueBuffer, cField) THEN
                    ASSIGN hField = phValueBuffer:BUFFER-FIELD (cField)
                           cValue = QUOTER (hField:BUFFER-VALUE) .
                ELSE IF VALID-OBJECT (poMissingValues) AND poMissingValues:ContainsKey (cField) THEN
                    ASSIGN cValue = QUOTER (poMissingValues:GetValue (cField)) .
                ELSE
                    UNDO, THROW NEW MissingIndexFieldValueException ("Unable to build unique find predicate. Value for index field &1.&2 is not provided."{&TRAN},
                                                                     phBuffer:NAME,
                                                                     cField) .

                    IF iField > 5 THEN
                        ASSIGN cQueryString = cQueryString + " AND ":U .

                    ASSIGN cQueryString = cQueryString +
                                          (IF pcBufferName > "":U THEN pcBufferName + ".":U ELSE "":U) +
                                          cField + " = ":U + cValue .
                END.

                LEAVE indexloop .
            END.
        END.

        RETURN cQueryString .

    END METHOD .

    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of primary unique columns
     * Notes:   Returns "" when there is no primary unique index.
     * @param phBuffer The handle of the buffer
     * @return The query string predicate
     */
    METHOD PUBLIC STATIC CHARACTER UniqueFindPredicate (phBuffer AS HANDLE):

        RETURN BufferHelper:UniqueFindPredicate (phBuffer,
                                                 TRUE) .

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 AND DEFINED (Infragistics) NE 0 &THEN
    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of primary unique columns
     *          Uses the buffer handle to determine the Unique Index fields, uses
     *          an UltraGrid's cell collection (i.e. of an UltraGridRow) to access
     *          the field/cell values
     * Notes:   Returns "" when there is no primary unique index.
     * @param phBuffer The Handle to the buffer to return the FIND predicate for
     * @param poCells The reference to the Cells collection (of an UltraGridRow) that contains the values
     * @param plIncludeBufferName Logical value indicating if the FIND predicate should be fully quallified including buffer names
     * @param plUseFullFieldNames Logical value indicating if the ProBindingSource is using the UseFullFieldNames feature
     * @return The query string predicate
     */
    METHOD PUBLIC STATIC CHARACTER UniqueFindPredicate (phBuffer AS HANDLE,
                                                        poCells AS Infragistics.Win.UltraWinGrid.CellsCollection,
                                                        plIncludeBufferName AS LOGICAL,
                                                        plUseFullFieldNames AS LOGICAL):

        DEFINE VARIABLE iIndex       AS INTEGER       NO-UNDO.
        DEFINE VARIABLE cIndex       AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE iField       AS INTEGER       NO-UNDO.
        DEFINE VARIABLE cField       AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE hField       AS HANDLE        NO-UNDO.
        DEFINE VARIABLE cQueryString AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE hIndexBuffer AS HANDLE        NO-UNDO.
        DEFINE VARIABLE oValue       AS System.Object NO-UNDO.

        DEFINE VARIABLE cKey         AS CHARACTER     NO-UNDO.

        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW AppError ("The method UniqueFindPredicate needs to be executed using a valid buffer handle."{&TRAN}, 0) .

        /* Mike Fechner, Consultingwerk Ltd. 26.11.2010
           When we are working on the BEFORE-BUFFER, the BEFORE-BUFFER has no
           indexes, then use the AFTER-BUFFER for the indexes */
        IF VALID-HANDLE (phBuffer:AFTER-BUFFER) THEN hIndexBuffer = phBuffer:AFTER-BUFFER .
        ELSE hIndexBuffer = phBuffer .

        indexloop: REPEAT ON ERROR UNDO, THROW:
            ASSIGN iIndex = iIndex + 1
                   cIndex = hIndexBuffer:INDEX-INFORMATION (iIndex) .

            IF cIndex > "":U THEN .
            ELSE LEAVE indexloop .

            IF ENTRY(2, cIndex) <> "1":U /* unique */ OR
               ENTRY(3, cIndex) <> "1":U /* primary */ THEN
               NEXT indexloop .

            ASSIGN cQueryString = "WHERE ":U .

            DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                ASSIGN cField = ENTRY(iField, cIndex)
                       hField = phBuffer:BUFFER-FIELD (cField) .

                IF iField > 5 THEN
                    ASSIGN cQueryString = cQueryString + " AND ":U .

                IF plUseFullFieldNames THEN
                    ASSIGN cKey = SUBSTITUTE ("&1:&2":U,
                                              phBuffer:NAME,
                                              hField:NAME) .
                ELSE
                    ASSIGN cKey = hField:NAME .

                oValue = poCells[cKey]:Value  .

                ASSIGN cQueryString = cQueryString +
                                      (IF plIncludeBufferName THEN phBuffer:NAME + ".":U ELSE "":U) +
                                      cField + " = ":U +

                                      (IF poCells:Exists (cKey) THEN
                                           QUOTER (UNBOX(oValue))
                                       ELSE "?":U) .
            END.

            RETURN cQueryString .
        END.

        IF cQueryString = "":U THEN DO:
            ASSIGN iIndex = 0 .

            indexloop: REPEAT ON ERROR UNDO, THROW:
                ASSIGN iIndex = iIndex + 1
                       cIndex = hIndexBuffer:INDEX-INFORMATION (iIndex) .

                IF cIndex > "":U THEN .
                ELSE LEAVE indexloop .

                IF ENTRY(2, cIndex) <> "1":U /* unique */ THEN
                   NEXT indexloop .

                ASSIGN cQueryString = "WHERE ":U .

                DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                    ASSIGN cField = ENTRY(iField, cIndex)
                           hField = phBuffer:BUFFER-FIELD (cField) .

                    IF iField > 5 THEN
                        ASSIGN cQueryString = cQueryString + " AND ":U .

                    IF plUseFullFieldNames THEN
                        ASSIGN cKey = SUBSTITUTE ("&1:&2":U,
                                                  phBuffer:NAME,
                                                  hField:NAME) .
                    ELSE
                        ASSIGN cKey = hField:NAME .

                    oValue = poCells[cKey]:Value  .

                    ASSIGN cQueryString = cQueryString +
                                          (IF plIncludeBufferName THEN phBuffer:NAME + ".":U ELSE "":U) +
                                          cField + " = ":U +

                                          (IF poCells:Exists (hField:NAME) THEN
                                               QUOTER (UNBOX(oValue))
                                           ELSE "?":U) .
                END.

                RETURN cQueryString .
            END.
        END.

        RETURN cQueryString .

    END METHOD .

    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of primary unique columns
     *          Uses the buffer handle to determine the Unique Index fields, uses
     *          an UltraGrid's cell collection (i.e. of an UltraGridRow) to access
     *          the field/cell values
     * Notes:   Returns "" when there is no primary unique index.
     * @param phBuffer The Handle to the buffer to return the FIND predicate for
     * @param poCells The reference to the Cells collection (of an UltraGridRow) that contains the values
     * @param poRow Not used, ignored
     * @param plIncludeBufferName Logical value indicating if the FIND predicate should be fully quallified including buffer names
     * @return The query string predicate
     */
    METHOD PUBLIC STATIC CHARACTER UniqueFindPredicate (phBuffer AS HANDLE,
                                                        poCells AS Infragistics.Win.UltraWinGrid.CellsCollection,
                                                        poRow AS Infragistics.Win.UltraWinGrid.UltraGridRow,
                                                        plIncludeBufferName AS LOGICAL):

        RETURN Consultingwerk.Util.BufferHelper:UniqueFindPredicate (phBuffer, poCells, TRUE, FALSE) .

    END METHOD .

    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of primary unique columns
     *          Uses the buffer handle to determine the Unique Index fields, uses
     *          an UltraGrid's cell collection (i.e. of an UltraGridRow) to access
     *          the field/cell values
     * Notes:   Returns "" when there is no primary unique index.
     * @param phBuffer The Handle to the buffer to return the FIND predicate for
     * @param poCells The reference to the Cells collection (of an UltraGridRow) that contains the values
     * @param plIncludeBufferName Logical value indicating if the FIND predicate should be fully quallified including buffer names
     * @return The query string predicate
     */
    METHOD PUBLIC STATIC CHARACTER UniqueFindPredicate (phBuffer AS HANDLE,
                                                        poCells AS Infragistics.Win.UltraWinGrid.CellsCollection,
                                                        plIncludeBufferName AS LOGICAL):

        RETURN Consultingwerk.Util.BufferHelper:UniqueFindPredicate (phBuffer, poCells, plIncludeBufferName, FALSE) .

     END METHOD .

    /**
     * Purpose: Builds a QueryString predicate that can be used to (re)fetch a
     *          record based on equality of primary unique columns
     *          Uses the buffer handle to determine the Unique Index fields, uses
     *          an UltraGrid's cell collection (i.e. of an UltraGridRow) to access
     *          the values
     * Notes:   Returns "" when there is no primary unique index.
     * @param phBuffer The Handle to the buffer to return the FIND predicate for
     * @param poCells The reference to the Cells collection (of an UltraGridRow) that contains the values
     * @return The query string predicate
     */
    METHOD PUBLIC STATIC CHARACTER UniqueFindPredicate (phBuffer AS HANDLE,
                                                        poCells AS Infragistics.Win.UltraWinGrid.CellsCollection):

        RETURN Consultingwerk.Util.BufferHelper:UniqueFindPredicate (phBuffer, poCells, TRUE, FALSE) .

    END METHOD.
&ENDIF

    /**
     * Purpose: Returns the fields of the Unique Primary of the table
     * Notes:   Same as UniqueKeyFields (phBuffer, TRUE, FALSE)
     * @param phBuffer The Buffer Handle
     * @return The a comma-delimited list of the unique fields of the table
     */
    METHOD PUBLIC STATIC CHARACTER UniqueKeyFields (phBuffer AS HANDLE):

        RETURN BufferHelper:UniqueKeyFields (phBuffer, TRUE, FALSE) .

    END METHOD .

    /**
     * Purpose: Returns the fields of the Unique Primary of the table
     * Notes:   Same as UniqueKeyFields (phBuffer, plPrimaryOnly, FALSE)
     * @param phBuffer The Buffer Handle
     * @param plPrimaryOnly Only return the fields of a primary unique index
     * @return The a comma-delimited list of the unique fields of the table
     */
    METHOD PUBLIC STATIC CHARACTER UniqueKeyFields (phBuffer AS HANDLE,
                                                    plPrimaryOnly AS LOGICAL):

        RETURN BufferHelper:UniqueKeyFields (phBuffer, plPrimaryOnly, FALSE) .

    END METHOD .

    /**
     * Purpose: Returns the fields of the Unique Primary of the table
     * Notes:
     * @param phBuffer The Buffer Handle
     * @param plPrimaryOnly Only return the fields of a primary unique index
     * @param plPrefixWithTable Prefix field names with Table/Buffer name
     * @return The a comma-delimited list of the unique fields of the table
     */
    METHOD PUBLIC STATIC CHARACTER UniqueKeyFields (phBuffer AS HANDLE,
                                                    plPrimaryOnly AS LOGICAL,
                                                    plPrefixWithTable AS LOGICAL):

        DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cIndex       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFields      AS CHARACTER NO-UNDO.

        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW AppError ("The method UniqueKeyFields needs to be executed using a valid buffer handle."{&TRAN}, 0) .

        indexloop: REPEAT ON ERROR UNDO, THROW:
            ASSIGN iIndex = iIndex + 1
                   cIndex = phBuffer:INDEX-INFORMATION (iIndex) .

            IF cIndex > "":U THEN .
            ELSE LEAVE indexloop .

            IF plPrimaryOnly THEN DO:
                IF ENTRY(2, cIndex) <> "1":U /* unique */ OR
                   ENTRY(3, cIndex) <> "1":U /* primary */ THEN
               NEXT indexloop .
            END.
            ELSE IF ENTRY(2, cIndex) <> "1":U /* unique */ THEN
                NEXT indexloop .

            DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                IF plPrefixWithTable THEN
                    ASSIGN cField = SUBSTITUTE ("&1.&2":U, phBuffer:NAME, ENTRY(iField, cIndex))
                           cFields = cFields + cField + ",":U .
                ELSE
                    ASSIGN cField = ENTRY(iField, cIndex)
                           cFields = cFields + cField + ",":U .
            END.

            RETURN TRIM (cFields, ",":U) .
        END.

        RETURN "":U .

    END METHOD.

    /**
     * Purpose: Builds and returns a unique record identifier composed from field
     *          values of either the primary unique index or the first unqiue index
     *          of the table
     * Notes:   Calls UniqueRecordIdentifier (phBuffer, TRUE, CHR(1))
     * @param phBuffer The handle of the buffer that contains the record
     * @return An identifier for the records based on unique index values
     */
    METHOD PUBLIC STATIC CHARACTER UniqueRecordIdentifier (phBuffer AS HANDLE):

        RETURN UniqueRecordIdentifier (phBuffer, TRUE, CHR(1)) .

    END METHOD .

    /**
     * Purpose: Builds and returns a unique record identifier composed from field
     *          values of either the primary unique index or the first unqiue index
     *          of the table
     * Notes:   The purpose of this method is to build a unique reusable identifier
     *          for a record that is not dependent on a ROWID (temp-table ROWID's are
     *          no suited record identifiers)
     * @param phBuffer The handle of the buffer that contains the record
     * @param plPrimaryOnly Logical value indicating if only the primary index should be used
     * @param pcDelimiter The delimiter to use for the field values
     * @return An identifier for the records based on unique index values
     */
    METHOD PUBLIC STATIC CHARACTER UniqueRecordIdentifier (phBuffer AS HANDLE,
                                                           plPrimaryOnly AS LOGICAL,
                                                           pcDelimiter AS CHARACTER):

        DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cIndex       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cIdentifier  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hIndexBuffer AS HANDLE    NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i  phBuffer WidgetTypeEnum:Buffer} .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .

        /* Mike Fechner, Consultingwerk Ltd. 26.11.2010
           When we are working on the BEFORE-BUFFER, the BEFORE-BUFFER has no
           indexes, then use the AFTER-BUFFER for the indexes */
        IF VALID-HANDLE (phBuffer:AFTER-BUFFER) THEN hIndexBuffer = phBuffer:AFTER-BUFFER .
        ELSE hIndexBuffer = phBuffer .

        indexloop: REPEAT ON ERROR UNDO, THROW:
            ASSIGN iIndex = iIndex + 1
                   cIndex = hIndexBuffer:INDEX-INFORMATION (iIndex) .

            IF cIndex > "":U THEN .
            ELSE LEAVE indexloop .

            IF ENTRY(2, cIndex) <> "1":U /* unique */ OR
               ENTRY(3, cIndex) <> "1":U /* primary */ THEN
               NEXT indexloop .

            DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                ASSIGN cField = ENTRY(iField, cIndex)
                       hField = phBuffer:BUFFER-FIELD (cField) .

                IF iField > 5 THEN
                    ASSIGN cIdentifier = cIdentifier + pcDelimiter .

                ASSIGN cIdentifier = cIdentifier + STRING (hField:BUFFER-VALUE).
            END.

            RETURN cIdentifier .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 08.01.2012
           If we get here , the table does not have a primary unique
           index, so try the first unique index */
        IF NOT plPrimaryOnly THEN DO:
            iIndex = 0 .

            indexloop: REPEAT ON ERROR UNDO, THROW:
                ASSIGN iIndex = iIndex + 1
                       cIndex = hIndexBuffer:INDEX-INFORMATION (iIndex) .

                IF cIndex > "":U THEN .
                ELSE LEAVE indexloop .

                IF ENTRY(2, cIndex) <> "1":U /* unique */ THEN
                   NEXT indexloop .

                DO iField = 5 TO NUM-ENTRIES (cIndex) BY 2 ON ERROR UNDO, THROW:
                    ASSIGN cField = ENTRY(iField, cIndex)
                           hField = phBuffer:BUFFER-FIELD (cField) .

                    IF iField > 5 THEN
                        ASSIGN cIdentifier = cIdentifier + pcDelimiter .

                    ASSIGN cIdentifier = cIdentifier + STRING (hField:BUFFER-VALUE).
                END.

                RETURN cIdentifier .
            END.
        END.

        UNDO, THROW NEW UniqueIndexRequiredException (phBuffer:NAME,
                                                      plPrimaryOnly) .

    END METHOD.

END CLASS.
