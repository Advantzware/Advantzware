/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartRelationService
    Purpose     : SmartFramerwork IRelationService implementation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 04 21:20:03 CEST 2013
    Notes       : Uses the following SmartMessages for RI related isuses:
                  VALMSG-5 OnChildUpdateRestrict (ensure that parent record exists)
                  VALMSG-3 OnParentDeleteRestrict (cannot delete parent because child exists)
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                 FROM PROPATH .
USING Consultingwerk.Exceptions.*                FROM PROPATH .
USING Consultingwerk.Framework.*                 FROM PROPATH .
USING Consultingwerk.OERA.*                      FROM PROPATH .
USING Consultingwerk.SmartFramework.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.Exceptions.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*     FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartRelationService
    IMPLEMENTS IRelationService:

    {Consultingwerk/SmartFramework/System/dsRelation.i}

    DEFINE VARIABLE lCachePopulated AS LOGICAL NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartRelationService class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartRelationService ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of a query (prepared, opened and located at the
                 first record) of all child records of the given child record
        Notes:
        @param phParentBuffer The handle of the database buffer with the parent record
        @param phChildBuffer The handle of the database buffer to add to the query
        @return The handle of a database query of the child records
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetChildRecordQueryOf (phParentBuffer AS HANDLE,
                                                phChildBuffer AS HANDLE):

        DEFINE BUFFER b_SmartRelation FOR eSmartRelation .

        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO.

        BufferAssert:IsDatabaseBuffer (phParentBuffer) .
        BufferAssert:IsDatabaseBuffer (phChildBuffer) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phParentBuffer} .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        /* Now locate the relation of the ChildBuffer */
        THIS-OBJECT:GetRelationForParentRecord (phParentBuffer,
                                                phChildBuffer,
                                                BUFFER b_SmartRelation) .

        ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1":U,
                                           b_SmartRelation.ChildTableName) .

        DO iField = 1 TO NUM-ENTRIES (b_SmartRelation.RelationFieldPair) BY 2:

            ASSIGN cQueryString = cQueryString +
                                  SUBSTITUTE (" &1 &2.&3 = &4":U,
                                              (IF iField = 1 THEN "WHERE":U ELSE "AND":U),
                                               b_SmartRelation.ChildTableName,
                                               ENTRY (iField + 1, b_SmartRelation.RelationFieldPair),
                                               QUOTER (phParentBuffer:BUFFER-FIELD (ENTRY (iField, b_SmartRelation.RelationFieldPair)):BUFFER-VALUE)) .
        END.

        RETURN QueryHelper:CreatePreparedQuery (phChildBuffer,
                                                cQueryString) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the first child record of the given child record
        Notes:
        @param phParentBuffer The handle of the database buffer with the parent record
        @param pcChildTableName The name of the database table with the child record
        @return The handle of a database buffer with the child record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetFirstChildRecordOf (phParentBuffer AS HANDLE,
                                                pcChildTableName AS CHARACTER):

        DEFINE VARIABLE hChildRecord AS HANDLE NO-UNDO .
        DEFINE VARIABLE hQuery       AS HANDLE NO-UNDO .

        BufferAssert:IsDatabaseBuffer (phParentBuffer) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phParentBuffer} .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        CREATE BUFFER hChildRecord FOR TABLE pcChildTableName .

        hQuery = THIS-OBJECT:GetChildRecordQueryOf (phParentBuffer,
                                                    hChildRecord) .

        RETURN hChildRecord .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the first parent record referenced by the given child field
        Notes:   The parent relation is identified by the child buffer name and the
                 relation based on the given child field name
        @param phChildBuffer The handle of the database buffer with the child record
        @param pcChildFieldName The unqualified field name of the child buffer that references the parent table
        @return The handle of a database buffer with the parent record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetParentRecordForField (phChildBuffer AS HANDLE,
                                                  pcChildFieldName AS CHARACTER):

        DEFINE BUFFER b_SmartRelation FOR eSmartRelation .

        DEFINE VARIABLE hParentRecord AS HANDLE NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE NO-UNDO .

        BufferAssert:IsDatabaseBuffer (phChildBuffer) .
        BufferAssert:HasField (phChildbuffer, pcChildFieldName) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phChildBuffer} .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        /* Now locate the relation of the ChildBuffer */
        THIS-OBJECT:GetRelationForChildRecord (phChildBuffer,
                                               pcChildFieldName,
                                               BUFFER b_SmartRelation) .

        CREATE BUFFER hParentRecord FOR TABLE b_SmartRelation.ParentTableName .

        hQuery = THIS-OBJECT:GetParentRecordQueryForField (phChildBuffer,
                                                           pcChildFieldName,
                                                           hParentRecord) .

        RETURN hParentRecord .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the first parent record of the given child record
        Notes:
        @param phChildBuffer The handle of the database buffer with the child record
        @return The handle of a database buffer with the parent record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetParentRecordOf (phChildBuffer AS HANDLE):

        DEFINE BUFFER b_SmartRelation FOR eSmartRelation .

        DEFINE VARIABLE hParentRecord AS HANDLE NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE NO-UNDO .

        BufferAssert:IsDatabaseBuffer (phChildBuffer) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phChildBuffer} .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        /* Now locate the relation of the ChildBuffer */
        THIS-OBJECT:GetRelationForChildRecord (phChildBuffer,
                                               BUFFER b_SmartRelation) .

        CREATE BUFFER hParentRecord FOR TABLE b_SmartRelation.ParentTableName .

        hQuery = THIS-OBJECT:GetParentRecordQueryOf (phChildBuffer,
                                                     hParentRecord) .

        RETURN hParentRecord .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the description value for the parent record referenced by
                 the given field.
        Notes:   The parent relation is identified by the child buffer name and the
                 relation based on the given child field name
        @param phChildBuffer The handle of the database buffer with the child record
        @param pcChildFieldName The unqualified field name of the child buffer that references the parent table
        @return The description value of the parent record as returned by the ITableInfoProvider.RecordDescription() method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetParentDescriptionForField (phChildBuffer AS HANDLE,
                                                          pcChildFieldName AS CHARACTER):

        DEFINE VARIABLE hParentBuffer      AS HANDLE             NO-UNDO .
        DEFINE VARIABLE oTableInfoProvider AS ITableInfoProvider NO-UNDO .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        oTableInfoProvider  = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITableInfoProvider} .

        Consultingwerk.Assertion.ObjectAssert:IsValid (oTableInfoProvider,
                                                       "ITableInfoProvider":U) .

        ASSIGN hParentBuffer = THIS-OBJECT:GetParentRecordForField (phChildBuffer,
                                                                    pcChildFieldName) .

        RETURN oTableInfoProvider:RecordDescription (hParentBuffer) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hParentBuffer) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the description value for the default parent record of the
                 given child buffer
        Notes:
        @param phChildBuffer The handle of the database buffer with the child record
        @return The description value of the parent record as returned by the ITableInfoProvider.RecordDescription() method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetParentDescriptionOf (phChildBuffer AS HANDLE):

        DEFINE VARIABLE hParentBuffer      AS HANDLE             NO-UNDO .
        DEFINE VARIABLE oTableInfoProvider AS ITableInfoProvider NO-UNDO .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        oTableInfoProvider  = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITableInfoProvider} .

        Consultingwerk.Assertion.ObjectAssert:IsValid (oTableInfoProvider,
                                                       "ITableInfoProvider":U) .

        ASSIGN hParentBuffer = THIS-OBJECT:GetParentRecordOf (phChildBuffer) .

        RETURN oTableInfoProvider:RecordDescription (hParentBuffer) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hParentBuffer) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of a query (prepared, opened and located at the
                 first record) of all parent records of the given child record
        Notes:
        @param phChildBuffer The handle of the database buffer with the child record
        @param pcChildFieldName The unqualified field name of the child buffer that references the parent table
        @param phParentBuffer The handle of the database buffer for the parent record, this buffer will be navigated by the returned query
        @return The handle of a database query of the parent records
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetParentRecordQueryForField (phChildBuffer AS HANDLE,
                                                       pcChildFieldName AS CHARACTER,
                                                       phParentBuffer AS HANDLE):

        DEFINE BUFFER b_SmartRelation FOR eSmartRelation .

        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.

        BufferAssert:IsDatabaseBuffer (phChildBuffer) .
        BufferAssert:HasField (phChildBuffer, pcChildFieldName) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phChildBuffer} .

        BufferAssert:IsDatabaseBuffer (phParentBuffer) .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        /* Now locate the relation of the ChildBuffer */
        THIS-OBJECT:GetRelationForChildRecord (phChildBuffer,
                                               pcChildFieldName,
                                               BUFFER b_SmartRelation) .

        Assert:Equals (b_SmartRelation.ParentTableName,
                       BufferHelper:QualifiedTableName (phParentBuffer)) .

        ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1 WHERE &1.&2 = &3":U,
                                           b_SmartRelation.ParentTableName,
                                           ENTRY (1, b_SmartRelation.RelationFieldPair),
                                           QUOTER (phChildBuffer:BUFFER-FIELD (pcChildFieldName):BUFFER-VALUE)) .

        RETURN QueryHelper:CreatePreparedQuery (phParentBuffer,
                                                cQueryString) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of a query (prepared, opened and located at the
                 first record) of all parent records of the given child record
        Notes:
        @param phChildBuffer The handle of the database buffer with the child record
        @param phParentBuffer The handle of the database buffer for the parent record, this buffer will be navigated by the returned query
        @return The handle of a database query of the parent records
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetParentRecordQueryOf (phChildBuffer AS HANDLE,
                                                 phParentBuffer AS HANDLE):

        DEFINE BUFFER b_SmartRelation FOR eSmartRelation .

        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iField       AS INTEGER   NO-UNDO .

        BufferAssert:IsDatabaseBuffer (phChildBuffer) .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phChildBuffer} .

        BufferAssert:IsDatabaseBuffer (phParentBuffer) .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        /* Now locate the relation of the ChildBuffer */
        THIS-OBJECT:GetRelationForParentRecord (phParentBuffer,
                                                phChildBuffer,
                                                BUFFER b_SmartRelation) .

        Assert:Equals (b_SmartRelation.ParentTableName,
                       BufferHelper:QualifiedTableName (phParentBuffer)) .

        ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1":U,
                                           b_SmartRelation.ParentTableName) .

        DO iField = 1 TO NUM-ENTRIES (b_SmartRelation.RelationFieldPair) BY 2:

            ASSIGN cQueryString = cQueryString +
                                  SUBSTITUTE (" &1 &2.&3 = &4":U,
                                              (IF iField = 1 THEN "WHERE":U ELSE "AND":U),
                                               b_SmartRelation.ParentTableName,
                                               ENTRY (iField, b_SmartRelation.RelationFieldPair),
                                               QUOTER (phChildBuffer:BUFFER-FIELD (ENTRY (iField + 1, b_SmartRelation.RelationFieldPair)):BUFFER-VALUE)) .
        END.

        RETURN QueryHelper:CreatePreparedQuery (phParentBuffer,
                                                cQueryString) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Locates the parent relation buffer for the given child buffer
                 and the
        Notes:
        @param phChildBuffer The handle of the database buffer with the child record
        @param pcChildFieldName The unqualified field name of the child buffer that references the parent table
        @param pb_SmartRelation The buffer for the eSmartRelation table
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetRelationForChildRecord (phChildBuffer AS HANDLE,
                                                     pcChildFieldName AS CHARACTER,
                                                     BUFFER pb_SmartRelation FOR eSmartRelation):

        DEFINE VARIABLE cQualifiedTableName AS CHARACTER NO-UNDO.

        BufferAssert:IsDatabaseBuffer (phChildBuffer) .
        BufferAssert:HasField (phChildBuffer, pcChildFieldName) .

        ASSIGN cQualifiedTableName = BufferHelper:QualifiedTableName (phChildBuffer) .

        FIND pb_SmartRelation WHERE pb_SmartRelation.ChildTableName = cQualifiedTableName
                                AND ENTRY (2, pb_SmartRelation.RelationFieldPair) = pcChildFieldName .

        CATCH err AS Progress.Lang.Error:
            IF NOT AVAILABLE pb_SmartRelation THEN
                UNDO, THROW NEW InvalidRelationException
                    (err,
                     SUBSTITUTE ("Invalid parent relation for &1.&2."{&TRAN},
                                 cQualifiedTableName,
                                 pcChildFieldName),
                     0) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Locates the default parent relation buffer for the given child buffer
        Notes:
        @param phChildBuffer The handle of the database buffer with the child record
        @param pb_SmartRelation The buffer for the eSmartRelation table
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetRelationForChildRecord (phChildBuffer AS HANDLE,
                                                     BUFFER pb_SmartRelation FOR eSmartRelation):

        DEFINE VARIABLE cQualifiedTableName AS CHARACTER NO-UNDO.

        BufferAssert:IsDatabaseBuffer (phChildBuffer) .

        ASSIGN cQualifiedTableName = BufferHelper:QualifiedTableName (phChildBuffer) .

        FIND pb_SmartRelation WHERE pb_SmartRelation.ChildTableName = cQualifiedTableName
                                AND pb_SmartRelation.DefaultParentRelation = TRUE .

        CATCH err AS Progress.Lang.Error:
            IF NOT AVAILABLE pb_SmartRelation THEN
                UNDO, THROW NEW InvalidRelationException
                    (err,
                     SUBSTITUTE ("Invalid default parent relation for &1."{&TRAN},
                                 cQualifiedTableName),
                     0) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Locates the child relation buffer for the given parent and child buffer
        Notes:
        @param phParentBuffer The handle of the database buffer with the parent record
        @param phChildBuffer The handle of the database buffer with the child record
        @param pb_SmartRelation The buffer for the eSmartRelation table
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetRelationForParentRecord (phParentBuffer AS HANDLE,
                                                      phChildBuffer AS HANDLE,
                                                      BUFFER pb_SmartRelation FOR eSmartRelation):

        DEFINE VARIABLE cParentQualifiedTableName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cChildQualifiedTableName  AS CHARACTER NO-UNDO.

        BufferAssert:IsDatabaseBuffer (phParentBuffer) .
        BufferAssert:IsDatabaseBuffer (phChildBuffer) .

        ASSIGN cParentQualifiedTableName = BufferHelper:QualifiedTableName (phParentBuffer)
               cChildQualifiedTableName  = BufferHelper:QualifiedTableName (phChildBuffer) .

        FIND pb_SmartRelation WHERE pb_SmartRelation.ParentTableName       = cParentQualifiedTableName
                                AND pb_SmartRelation.ChildTableName        = cChildQualifiedTableName
                                AND pb_SmartRelation.DefaultParentRelation = TRUE
                                NO-ERROR .

        IF NOT AVAILABLE pb_SmartRelation THEN
            FIND FIRST pb_SmartRelation WHERE pb_SmartRelation.ParentTableName = cParentQualifiedTableName
                                          AND pb_SmartRelation.ChildTableName  = cChildQualifiedTableName .

        CATCH err AS Progress.Lang.Error:
            IF NOT AVAILABLE pb_SmartRelation THEN
                UNDO, THROW NEW InvalidRelationException
                    (err,
                     SUBSTITUTE ("Invalid relation from &1 to &2."{&TRAN},
                                 cParentQualifiedTableName,
                                 cChildQualifiedTableName),
                     0) .
        END CATCH.

    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Processes the RI rule OnChildUpdateRestrict
        Notes:   Ensures that a parent record exists for parent cardinality 1 or 1n
        @param phChildBuffer The handle of the child buffer
        @param pcParentTableName The name of the parent buffer
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnChildUpdateRestrict (phChildBuffer AS HANDLE,
                                                 pcParentTableName AS CHARACTER):

        DEFINE VARIABLE hParentQuery  AS HANDLE NO-UNDO .
        DEFINE VARIABLE hParentBuffer AS HANDLE NO-UNDO .

        DEFINE BUFFER b_SmartRelation FOR eSmartRelation .

        CREATE BUFFER hParentBuffer FOR TABLE pcParentTableName .

        hParentQuery = THIS-OBJECT:GetParentRecordQueryOf (phChildBuffer,
                                                           hParentBuffer) .

        IF NOT hParentBuffer:AVAILABLE THEN DO:
            THIS-OBJECT:GetRelationForParentRecord (hParentBuffer,
                                                    phChildBuffer,
                                                    BUFFER b_SmartRelation) .

            UNDO, THROW NEW ReferentialIntegrityException (MessageFormatter:GetMessage ("VALMSG":U,
                                                                                        5,
                                                                                        (IF b_SmartRelation.ChildRoleDescription > "":U
                                                                                                THEN b_SmartRelation.ChildRoleDescription
                                                                                                ELSE ENTRY (NUM-ENTRIES (b_SmartRelation.ChildTableName, ".":U), b_SmartRelation.ChildTableName, ".":U)),
                                                                                        (IF b_SmartRelation.ParentRoleDescription > "":U
                                                                                                THEN b_SmartRelation.ParentRoleDescription
                                                                                                ELSE ENTRY (NUM-ENTRIES (b_SmartRelation.ParentTableName, ".":U), b_SmartRelation.ParentTableName, ".":U))),
                                                           0) .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hParentQuery) .
            GarbageCollectorHelper:DeleteObject (hParentBuffer) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Processes the RI rule OnParentDeleteCascade
        Notes:
        @param phParentBuffer The handle of the parent buffer
        @param pcChildTableName The name of the child buffer
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnParentDeleteCascade (phParentBuffer AS HANDLE,
                                                 pcChildTableName AS CHARACTER):

        DEFINE VARIABLE hChildQuery  AS HANDLE NO-UNDO .
        DEFINE VARIABLE hChildBuffer AS HANDLE NO-UNDO .

        CREATE BUFFER hChildBuffer FOR TABLE pcChildTableName .

        hChildQuery = THIS-OBJECT:GetChildRecordQueryOf (phParentBuffer,
                                                         hChildBuffer) .

        hChildQuery:GET-FIRST (EXCLUSIVE-LOCK) .

        DO WHILE NOT hChildQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

            THIS-OBJECT:ProcessDelete (hChildBuffer, TRUE) .

            hChildQuery:GET-NEXT (EXCLUSIVE-LOCK) .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hChildQuery) .
            GarbageCollectorHelper:DeleteObject (hChildBuffer) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes the RI rule OnParentDeleteNullify
        Notes:
        @param phParentBuffer The handle of the parent buffer
        @param pcChildTableName The name of the child buffer
        @param pcRelationFields The field pairs for the data relation
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnParentDeleteNullify (phParentBuffer AS HANDLE,
                                                 pcChildTableName AS CHARACTER,
                                                 pcRelationFields AS CHARACTER):

        DEFINE VARIABLE hChildQuery  AS HANDLE NO-UNDO .
        DEFINE VARIABLE hChildBuffer AS HANDLE NO-UNDO .

        DEFINE VARIABLE iField AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField AS HANDLE    NO-UNDO.

        CREATE BUFFER hChildBuffer FOR TABLE pcChildTableName .

        hChildQuery = THIS-OBJECT:GetChildRecordQueryOf (phParentBuffer,
                                                         hChildBuffer) .

        hChildQuery:GET-FIRST (EXCLUSIVE-LOCK) .

        DO WHILE NOT hChildQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
            DO iField = 2 TO NUM-ENTRIES (pcRelationFields) BY 2 ON ERROR UNDO, THROW:

                ASSIGN cField = ENTRY (iField, pcRelationFields) .

                IF BufferHelper:HasField (hChildBuffer, cField) THEN DO:
                    ASSIGN hField = hChildBuffer:BUFFER-FIELD (cField)

                           hField:BUFFER-VALUE = ? .
                END.
            END.

            hChildQuery:GET-NEXT (EXCLUSIVE-LOCK) .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hChildQuery) .
            GarbageCollectorHelper:DeleteObject (hChildBuffer) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes the RI rule OnParentDeleteRestrict
        Notes:
        @param phParentBuffer The handle of the parent buffer
        @param pcChildTableName The name of the child buffer
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnParentDeleteRestrict (phParentBuffer AS HANDLE,
                                                  pcChildTableName AS CHARACTER):

        DEFINE VARIABLE hChildBuffer AS HANDLE NO-UNDO.

        DEFINE BUFFER b_SmartRelation FOR eSmartRelation .

        ASSIGN hChildBuffer = THIS-OBJECT:GetFirstChildRecordOf (phParentBuffer, pcChildTableName) .

        IF hChildBuffer:AVAILABLE THEN DO:
        /* Now locate the relation of the ChildBuffer */
            THIS-OBJECT:GetRelationForParentRecord (phParentBuffer,
                                                    hChildBuffer,
                                                    BUFFER b_SmartRelation) .

            UNDO, THROW NEW ReferentialIntegrityException (MessageFormatter:GetMessage ("VALMSG":U,
                                                                                        3,
                                                                                        (IF b_SmartRelation.ParentRoleDescription > "":U
                                                                                                THEN b_SmartRelation.ParentRoleDescription
                                                                                                ELSE ENTRY (NUM-ENTRIES (b_SmartRelation.ParentTableName, ".":U), b_SmartRelation.ParentTableName, ".":U)),
                                                                                        (IF b_SmartRelation.ChildRoleDescription > "":U
                                                                                                THEN b_SmartRelation.ChildRoleDescription
                                                                                                ELSE ENTRY (NUM-ENTRIES (b_SmartRelation.ChildTableName, ".":U), b_SmartRelation.ChildTableName, ".":U))),
                                                           0) .
        END.


        FINALLY:
            GarbageCollectorHelper:DeleteObject (hChildBuffer) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes the delete of a parent buffer
        Notes:   On delete restrict, on delete cascase
                 Optionally deletes the parent buffer as well
        @param phBuffer The handle of the database buffer (parent) to process delete for
        @param plDeleteParent Logical parameter, indicating of the parent buffer should actually be deleted
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ProcessDelete (phBuffer AS HANDLE,
                                      plDeleteParent AS LOGICAL):

        DEFINE VARIABLE cTableName   AS CHARACTER NO-UNDO .

        DEFINE BUFFER eSmartRelation FOR eSmartRelation .

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .
        BufferAssert:IsDatabaseBuffer (phBuffer) .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        ASSIGN cTableName = BufferHelper:QualifiedTableName (phBuffer) .

        FOR EACH eSmartRelation WHERE eSmartRelation.ParentTableName = cTableName
                                  AND eSmartRelation.Inactive        = FALSE ON ERROR UNDO, THROW:

            CASE eSmartRelation.OnParentDelete:

                WHEN "Cascade":U THEN
                    OnParentDeleteCascade (phBuffer, eSmartRelation.ChildTableName) .
                WHEN "Restrict":U THEN
                    OnParentDeleteRestrict (phBuffer, eSmartRelation.ChildTableName) .
                WHEN "Nullify":U THEN
                    OnParentDeleteNullify (phBuffer, eSmartRelation.ChildTableName,
                                           eSmartRelation.RelationFieldPair) .

            END CASE .
        END.

        IF plDeleteParent THEN
            phBuffer:BUFFER-DELETE () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes the write of a child buffer
        Notes:
        @param phBuffer The handle of the database buffer (parent) to process the write for
        @param plNewRecord Logical parameter, indicating if the record has been created or updated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ProcessWrite (phBuffer AS HANDLE,
                                     plNewRecord AS LOGICAL):


        DEFINE VARIABLE cTableName   AS CHARACTER NO-UNDO .

        DEFINE BUFFER eSmartRelation FOR eSmartRelation .

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .
        BufferAssert:IsDatabaseBuffer (phBuffer) .

        IF NOT lCachePopulated THEN
            THIS-OBJECT:RefreshCache () .

        ASSIGN cTableName = BufferHelper:QualifiedTableName (phBuffer) .

        FOR EACH eSmartRelation WHERE eSmartRelation.ChildTableName = cTableName
                                  AND eSmartRelation.Inactive        = FALSE ON ERROR UNDO, THROW:

            IF eSmartRelation.ParentCardinality = "1":U OR
               eSmartRelation.ParentCardinality = "1n":U THEN

                OnChildUpdateRestrict (phBuffer, eSmartRelation.ParentTableName) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the Cache used by the service
        Notes:   Caches data from the Consultingwerk.SmartFramework.System.RelationBusinessEntity
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RefreshCache ():

        DEFINE VARIABLE oRequest AS IFetchDataRequest NO-UNDO .

        DATASET dsRelation:EMPTY-DATASET () .

        ASSIGN lCachePopulated = TRUE
               oRequest        = NEW FetchDataRequest ("eSmartRelation":U) .

        IF NOT CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                           "Consultingwerk.SmartFramework.System.RelationBusinessEntity":U,
                                                           oRequest,
                                                           OUTPUT DATASET dsRelation) .
        ELSE
            ServiceInterface:FetchData ("Consultingwerk.SmartFramework.System.RelationBusinessEntity":U,
                                        oRequest,
                                        OUTPUT DATASET dsRelation) .

    END METHOD .

END CLASS.
