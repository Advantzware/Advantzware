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
    File        : SmartDataAccess
    Purpose     : Enhanced base class for Business Entities that provides
                  SmartFramework functionality like attachments and comments
    Syntax      : Abstract base class
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 13 11:12:22 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Framework.Session.*     FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
&IF DEFINED (SmartFramework) NE 0 &THEN
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
&ENDIF
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.SmartDataAccess
    INHERITS DataAccess
    IMPLEMENTS IProvidesSmartRecordInformation ABSTRACT:

    /**
     * Purpose: The dictionary of SourceTables
     * Notes:   The source tables defines the name of the DB tables which is used
     *          for hooking into the SmartFramework functionality like Attachments
     *          and comments.
     *          By deftault the first DATA-SOURCE table is used. However there may
     *          be cases when this needs to be overridden (e.g. when using another
     *          tables as the first member in a query to optimize read access)
     *          Entries should be in the form of "<databasename.talbename>"
     */
    DEFINE PROTECTED PROPERTY SourceTables AS CharacterDictionary NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Track deletions in SmartDeletion table
     * Notes:   Property is PUBLIC, as the SmartBusinessEntity may assign it's value
     */
    DEFINE PUBLIC PROPERTY TrackDeletions AS LOGICAL NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor of the SmartDataAccess class
     * Notes:
     * @param phDataset The handle of the Business Entity dataset
     */
    CONSTRUCTOR PUBLIC SmartDataAccess (INPUT phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        THIS-OBJECT:SourceTables = NEW CharacterDictionary () .

    END CONSTRUCTOR.

    /**
     * Purpose: Invoked by DataAccess:CommitChanges within the database transaction
     *          to optionally assign key field values for new records
     * Notes:   Assigns key field values for new records based on the
     *          SmartTable.KeyFieldAssignmentGUID setting
     * @param phBeforeBuffer The handle of the before buffer
     */
    METHOD OVERRIDE PROTECTED VOID AssignKeyFieldValues (INPUT phBeforeBuffer AS HANDLE):

        DEFINE VARIABLE hBuffer      AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cSourceTable AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cKeyFields   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER   NO-UNDO .
        DEFINE VARIABLE j            AS INTEGER   NO-UNDO .

&IF DEFINED (SmartFramework) NE 0 &THEN
        DEFINE BUFFER SmartTable FOR SmartTable .

        DEFINE VARIABLE oService AS IAssignKeyFieldValuesService NO-UNDO .

        bufferLoop:
        DO i = 1 TO THIS-OBJECT:DatasetHandle:NUM-BUFFERS:
            ASSIGN hBuffer      = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (i) .

            IF NOT VALID-HANDLE (hBuffer:DATA-SOURCE) THEN
                NEXT bufferLoop.

            ASSIGN cSourceTable = THIS-OBJECT:SourceTable (hBuffer:NAME).

            IF NUM-ENTRIES (cSourceTable, ".":U) = 2 THEN DO ON ERROR UNDO, THROW:
                FIND SmartTable WHERE SmartTable.DatabaseName = ENTRY (1, cSourceTable, ".":U)
                                  AND SmartTable.TableName    = ENTRY (2, cSourceTable, ".":U) NO-LOCK NO-ERROR .

                IF NOT AVAILABLE SmartTable THEN
                    NEXT bufferLoop .

                IF CharacterType:IsNullOrEmpty (SmartTable.KeyFieldAssignmentGUID) THEN
                    NEXT bufferLoop .

                ASSIGN cKeyFields = "":U .

                DO j = 1 TO NUM-ENTRIES (SmartTable.UniqueKeyFields):
                    cKeyFields = cKeyFields +
                                 (IF j > 1 THEN ",":U ELSE "":U) +
                                 THIS-OBJECT:TargetColumn (SmartTable.TableName,
                                                           ENTRY (j, SmartTable.UniqueKeyFields),
                                                           hBuffer:DATA-SOURCE) .
                END.

                IF CharacterType:IsNullOrEmpty (cKeyFields) THEN
                    NEXT bufferLoop .

                BufferHelper:FindAfterBuffer (phBeforeBuffer) .

                oService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IAssignKeyFieldValuesService
                                                         "NEW Consultingwerk.SmartFramework.System.AssignKeyFieldValuesService()"} .

                oService:AssignKeyFields (SmartTable.KeyFieldAssignmentGUID,
                                          hBuffer,
                                          cKeyFields) .
            END.
        END.
&ENDIF

    END METHOD .

    /**
     * Purpose: Assign SmartRecordKey and Attachment/Comment function
     * Notes:
     */
    METHOD PUBLIC VOID AssignSmartRecordInformation ():

        DEFINE VARIABLE i            AS INTEGER   NO-UNDO .
        DEFINE VARIABLE j            AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hBuffer      AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cSourceTable AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cKeyFields   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cKeyValue    AS CHARACTER NO-UNDO .

&IF DEFINED (SmartFramework) NE 0 &THEN
        DEFINE BUFFER SmartTable FOR SmartTable .

        bufferLoop:
        DO i = 1 TO THIS-OBJECT:DatasetHandle:NUM-BUFFERS:
            ASSIGN hBuffer      = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (i) .

            IF NOT VALID-HANDLE (hBuffer:DATA-SOURCE) THEN
                NEXT bufferLoop.

            ASSIGN cSourceTable = THIS-OBJECT:SourceTable (hBuffer:NAME).

            IF NOT BufferHelper:HasField (hBuffer, "SmartRecordKey":U) THEN
                NEXT bufferLoop .

            IF NUM-ENTRIES (cSourceTable, ".":U) = 2 THEN DO ON ERROR UNDO, THROW:
                FIND SmartTable WHERE SmartTable.DatabaseName = ENTRY (1, cSourceTable, ".":U)
                                  AND SmartTable.TableName    = ENTRY (2, cSourceTable, ".":U) NO-LOCK NO-ERROR .

                IF NOT AVAILABLE SmartTable THEN
                    NEXT bufferLoop .

                ASSIGN cKeyFields = "":U .

                DO j = 1 TO NUM-ENTRIES (SmartTable.UniqueKeyFields):
                    cKeyFields = cKeyFields +
                                 (IF j > 1 THEN ",":U ELSE "":U) +
                                 THIS-OBJECT:TargetColumn (SmartTable.TableName,
                                                           ENTRY (j, SmartTable.UniqueKeyFields),
                                                           hBuffer:DATA-SOURCE) .
                END.

                IF CharacterType:IsNullOrEmpty (cKeyFields) THEN
                    NEXT bufferLoop .

                hQuery = QueryHelper:CreatePreparedQuery (hBuffer) .

                DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

                    ASSIGN cKeyValue = BufferHelper:BufferFieldSubstituteFormatted (hBuffer,
                                                                                    cKeyFields,
                                                                                    SmartTable.UniqueKeySubstitute,
                                                                                    SmartTable.UniqueKeyFormats)
                           hBuffer::SmartRecordKey = cKeyValue .

                    IF SmartTable.AllowAttachments AND
                       BufferHelper:HasField (hBuffer, "SmartAttachments":U) THEN

                        ASSIGN hBuffer::SmartAttachments = CAN-FIND (FIRST SmartAttachment WHERE SmartAttachment.TableGUID = SmartTable.TableGUID
                                                                                            AND SmartAttachment.RecordKeyValue = cKeyValue
                                                                                            AND SmartAttachment.AttachmentRoleGUID > "":U) .
                    IF SmartTable.AllowComments AND
                       BufferHelper:HasField (hBuffer, "SmartComments":U) THEN

                        ASSIGN hBuffer::SmartComments = CAN-FIND (FIRST SmartAttachment WHERE SmartAttachment.TableGUID = SmartTable.TableGUID
                                                                                          AND SmartAttachment.RecordKeyValue = cKeyValue
                                                                                          AND SmartAttachment.CommentRoleGUID > "":U) .

                    hQuery:GET-NEXT () .
                END.

                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hQuery) .
                END FINALLY.
            END. /* 2 entries */
        END.
&ENDIF

    END METHOD .

    /**
     * Purpose: This procedure is called from saveChanges in the DA
     *          to do the actual database commit and any transaction-related
     *          validation logic for a buffer.
     * Notes:   Override to CommitChanges in the DataAccess class. Performs cascading
     *          deletion of SmartAttachment records
     *          Uses Consultingwerk.SmartFramework.IRelationService to ensure
     *          referential integrity (Delete Restrict, Delete Cascade)
     * @param phBuffer The handle of the buffer to commit
     */
    METHOD OVERRIDE PROTECTED VOID CommitChanges (phBuffer AS HANDLE):

        DEFINE VARIABLE cSourceTable AS CHARACTER NO-UNDO .
        DEFINE VARIABLE lDeleted     AS LOGICAL   NO-UNDO INIT FALSE .
        DEFINE VARIABLE hBeforeBuff  AS HANDLE    NO-UNDO .
        DEFINE VARIABLE iRowState    AS INTEGER   NO-UNDO .
        DEFINE VARIABLE roRowid      AS ROWID     NO-UNDO .

&IF DEFINED (SmartFramework) NE 0 &THEN
        DEFINE BUFFER SmartAttachment FOR SmartAttachment .
        DEFINE BUFFER SmartTable      FOR SmartTable .
        DEFINE BUFFER SmartDeletion   FOR SmartDeletion .

        DEFINE VARIABLE oRelationService AS Consultingwerk.SmartFramework.IRelationService NO-UNDO .
        DEFINE VARIABLE oTableInfoProvider AS Consultingwerk.SmartFramework.ITableInfoProvider NO-UNDO .

        DEFINE VARIABLE cKeyValues AS CHARACTER NO-UNDO.

        ASSIGN hBeforeBuff = phBuffer:BEFORE-BUFFER
               iRowState   = hBeforeBuff:ROW-STATE
               roRowid     = hBeforeBuff:AFTER-ROWID .

        IF hBeforeBuff:ROW-STATE = ROW-DELETED THEN DO:
            ASSIGN lDeleted = TRUE .

            oRelationService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IRelationService} .
            oTableInfoProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITableInfoProvider} .

            IF VALID-OBJECT (oRelationService) AND VALID-HANDLE (phBuffer:DATA-SOURCE) AND
               phBuffer:DATA-SOURCE:NUM-SOURCE-BUFFERS > 0 THEN DO:

                hBeforeBuff:AFTER-BUFFER:BUFFER-RELEASE () .
                phBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (1):BUFFER-RELEASE () .

                phBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (1):FIND-FIRST
                    (phBuffer:DATA-SOURCE:SAVE-WHERE-STRING, EXCLUSIVE-LOCK) NO-ERROR  .

                IF THIS-OBJECT:TrackDeletions THEN
                    ASSIGN cKeyValues = oTableInfoProvider:RecordKeyFields (phBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (1)) .

                IF phBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (1):AVAILABLE THEN DO ON ERROR UNDO, THROW:
                    oRelationService:ProcessDelete (phBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (1),
                                                    FALSE) .

                    CATCH err AS Progress.Lang.Error:
                        DatasetHelper:AddErrorString (hBeforeBuff,
                                                      err:GetMessage (1)) .

                        IF FrameworkSettings:DebugMode AND err:CallStack > "":U THEN
                            DatasetHelper:AddErrorString (hBeforeBuff,
                                                          SUBSTITUTE ("~nBackend-Stacktrace:~n&1~n":U, STRING (ErrorHelper:StackTrace (err)))) .
                    END CATCH.
                END .
            END .
        END .
&ENDIF

        SUPER:CommitChanges(INPUT phBuffer).

&IF DEFINED (SmartFramework) NE 0 &THEN
        /* Mike Fechner, Consultingwerk Ltd. 24.08.2015
           SCL-979: Don't perform post commit processing when row undone
           due to validation error */
        IF phBuffer:BEFORE-BUFFER:ERROR THEN
            RETURN .

        IF iRowState = ROW-DELETED THEN DO:

            IF NOT lDeleted THEN
                RETURN .

            ASSIGN cSourceTable = THIS-OBJECT:SourceTable (phBuffer:NAME) .

            IF THIS-OBJECT:TrackDeletions THEN DO:
                CREATE SmartDeletion .
                ASSIGN SmartDeletion.DeletionTable          = cSourceTable
                       SmartDeletion.DeletionKeyFieldValues = cKeyValues .
                RELEASE SmartDeletion.
            END.

            IF NOT BufferHelper:HasField (phBuffer, "SmartRecordKey":U) THEN
                RETURN .

            FIND SmartTable WHERE SmartTable.DatabaseName = ENTRY (1, cSourceTable, ".":U)
                              AND SmartTable.TableName    = ENTRY (2, cSourceTable, ".":U) NO-LOCK NO-ERROR .

            IF NOT AVAILABLE SmartTable THEN
                RETURN .

            FOR EACH SmartAttachment WHERE SmartAttachment.TableGUID          = SmartTable.TableGUID
                                       AND SmartAttachment.RecordKeyValue     = hBeforeBuff::SmartRecordKey
                                   EXCLUSIVE-LOCK ON ERROR UNDO, THROW:

                DELETE SmartAttachment .
            END.
        END.
        ELSE IF iRowstate = ROW-MODIFIED OR iRowState = ROW-CREATED THEN DO ON ERROR UNDO, THROW:

            oRelationService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IRelationService} .

            IF VALID-OBJECT (oRelationService) AND VALID-HANDLE (phBuffer:DATA-SOURCE) AND
               phBuffer:DATA-SOURCE:NUM-SOURCE-BUFFERS > 0 THEN DO:

                phBuffer:FIND-BY-ROWID (roRowid) NO-ERROR .

                THIS-OBJECT:FindSourceBuffer (phBuffer, FALSE)   .

                IF phBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (1):AVAILABLE THEN

                    oRelationService:ProcessWrite (phBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (1),
                                                   IF iRowState = ROW-CREATED THEN TRUE ELSE FALSE) .
            END.

            CATCH err AS Progress.Lang.Error:
                DatasetHelper:AddErrorString (hBeforeBuff,
                                              err:GetMessage (1)) .

                UNDO, RETURN .
            END CATCH.
        END.
&ENDIF

    END METHOD.

&IF DEFINED (SmartFramework) NE 0 &THEN
    /**
     * Purpose: Resets a modification for the given table and key field values
     * Notes:
     * @param pcModifiedTable The name of the database table to reset the modification for
     * @param pcKeyFieldValues The values of the key fields to reset the modification for
     */
    METHOD PUBLIC VOID ResetModifiedState (pcModifiedTable AS CHARACTER,
                                           pcKeyFieldValues AS CHARACTER):

        DEFINE BUFFER SmartModifiedState FOR SmartModifiedState .

        IF CAN-FIND (SmartModifiedState WHERE SmartModifiedState.ModifiedStateTable          = pcModifiedTable
                                          AND SmartModifiedState.ModifiedStateKeyFieldValues = pcKeyFieldValues) THEN DO TRANSACTION:
            FIND SmartModifiedState WHERE SmartModifiedState.ModifiedStateTable          = pcModifiedTable
                                      AND SmartModifiedState.ModifiedStateKeyFieldValues = pcKeyFieldValues EXCLUSIVE-LOCK NO-ERROR .

            DELETE SmartModifiedState .
        END.

    END METHOD.
&ENDIF

    /**
     * Purpose: Returns the name of the source (database) table for a ProDataset
     *          table name
     * Notes:
     * @param pcTempTableName The temp-table buffer name to return the source table for
     * @return The database table name of the source table
     */
    METHOD PUBLIC CHARACTER SourceTable (pcTempTableName AS CHARACTER):

        DEFINE VARIABLE hDatasetBuffer    AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hSourceBuffer     AS HANDLE  NO-UNDO.
        DEFINE VARIABLE lDetachDataSource AS LOGICAL NO-UNDO INIT FALSE .

        IF NOT THIS-OBJECT:DataSourcesAttached THEN DO:
            THIS-OBJECT:AttachDataSources () .
            lDetachDataSource = TRUE .
        END.

        Consultingwerk.Assertion.DatasetAssert:HasBuffer (THIS-OBJECT:DatasetHandle,
                                                          pcTempTableName) .

        IF THIS-OBJECT:SourceTables:ContainsKey (pcTempTableName) THEN
            RETURN THIS-OBJECT:SourceTables:GetValue (pcTempTableName) .

        ASSIGN hDatasetBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (pcTempTableName) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hDatasetBuffer:DATA-SOURCE """Temp-Table Buffer DATA-SOURCE"":U"} .

        ASSIGN hSourceBuffer = hDatasetBuffer:DATA-SOURCE:GET-SOURCE-BUFFER (1) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hSourceBuffer """DATA-SOURCE Buffer handle"":U"} .

        RETURN SUBSTITUTE ("&1.&2":U,
                           hSourceBuffer:DBNAME,
                           hSourceBuffer:TABLE) .

        FINALLY:
            IF lDetachDataSource = TRUE THEN
                THIS-OBJECT:DetachDataSources() .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Returns the temp-table field name matching a database field name
     * Notes:   The database table specified is the source table of the temp-table
     *          This method should be overridden when the default functionality does
     *          not return the proper field mapping (complex joined queries)
     * @param pcDatabaseTable The name of the source database table
     * @param pcColumn The name of the database field to return the mapped temp-table for
     * @param phDataSource The data source with the established mapping
     * @return The target (temp-table) field name
     */
    METHOD PROTECTED CHARACTER TargetColumn (pcDatabaseTable AS CHARACTER,
                                             pcColumn AS CHARACTER,
                                             phDataSource AS HANDLE):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.

        cMap = phDataSource:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

        RETURN ENTRY(2,
                     ENTRY (LOOKUP(SUBSTITUTE ("&1.&2":U,
                                               pcDatabaseTable,
                                               pcColumn), cMap) - 1, cMap),
                     ".":U) .
    END METHOD .

&IF DEFINED (SmartFramework) NE 0 &THEN
    /**
     * Purpose: Tracks a modification for the given table and key field values
     * Notes:
     * @param pcModifiedTable The name of the database table to track the modification for
     * @param pcKeyFieldValues The values of the key fields to track the modification for
     */
    METHOD PUBLIC VOID TrackModifiedState (pcModifiedTable AS CHARACTER,
                                           pcKeyFieldValues AS CHARACTER):

        DEFINE BUFFER SmartModifiedState FOR SmartModifiedState .

        IF CAN-FIND (SmartModifiedState WHERE SmartModifiedState.ModifiedStateTable          = pcModifiedTable
                                          AND SmartModifiedState.ModifiedStateKeyFieldValues = pcKeyFieldValues) THEN DO TRANSACTION:
            FIND SmartModifiedState WHERE SmartModifiedState.ModifiedStateTable          = pcModifiedTable
                                      AND SmartModifiedState.ModifiedStateKeyFieldValues = pcKeyFieldValues EXCLUSIVE-LOCK NO-ERROR .

            ASSIGN SmartModifiedState.ModifiedStateTimeStamp = NOW
                   SmartModifiedState.ModifiedStateUser      = SessionManager:UserName .

            RELEASE SmartModifiedState .
        END.
        ELSE DO TRANSACTION:
            CREATE SmartModifiedState .

            ASSIGN SmartModifiedState.ModifiedStateTable          = pcModifiedTable
                   SmartModifiedState.ModifiedStateKeyFieldValues = pcKeyFieldValues
                   SmartModifiedState.ModifiedStateTimeStamp      = NOW
                   SmartModifiedState.ModifiedStateUser           = SessionManager:UserName .

            RELEASE SmartModifiedState .
        END.

    END METHOD.
&ENDIF

END CLASS.
