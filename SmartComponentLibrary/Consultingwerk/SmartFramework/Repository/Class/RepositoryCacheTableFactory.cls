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
    File        : RepositoryCacheTableFactory
    Purpose     : Services that generates the repository cache temp-tables
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 25 22:37:40 CET 2016
    Notes       : Only returns attributes with the AppliesAtRuntime flag
                  set to true
                  Uses direct database access for the AttributeValue table
                  Supports the SmartRepositoryService custom log entry type
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.Framework.Collections.*           FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*       FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Class.RepositoryCacheTableFactory
    IMPLEMENTS IRepositoryCacheTableFactory:

    DEFINE TEMP-TABLE ttAttributesCache NO-UNDO
        FIELD ClassName  AS CHARACTER
        FIELD CacheType  AS CHARACTER /* Consultingwerk.SmartFramework.Repository.Class.GetAttributesForEnum */
        FIELD Attributes AS HANDLE
        INDEX CacheTypeClassName IS UNIQUE CacheType ClassName .

    DEFINE VARIABLE lLogging AS LOGICAL NO-UNDO INITIAL FALSE .

    /**
     * Purpose: Constructor for the RepositoryCacheTableFactory class
     * Notes:
     */
    CONSTRUCTOR PUBLIC RepositoryCacheTableFactory ():

        ASSIGN lLogging = LogManager:IsCustomLogEntryTypeActive ("SmartRepositoryService":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds the field from the given object type to the attribute temp-table
        Notes:   Uses direct database access for the attribute value records
        @param pcObjectTypeGuid The GUID of the object type to add fields from
        @param phTable The handle of the temp-table to add fields to
        @param pcFieldNames The comma-delimited list of field names that have already been added to the temp-table
        @param poAttributesFrom The GetAttributesForEnum indicating if attributes should be added for runtime or design
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddFieldsFromObjectType (pcObjectTypeGuid AS CHARACTER,
                                                   phTable AS HANDLE,
                                                   INPUT-OUTPUT pcFieldNames AS CHARACTER,
                                                   poAttributesFrom AS GetAttributesForEnum):

        DEFINE QUERY qQuery FOR SmartAttributeValue, SmartAttribute .

        IF poAttributesFrom = GetAttributesForEnum:Runtime THEN
            OPEN QUERY qQuery
                FOR EACH SmartAttributeValue WHERE SmartAttributeValue.ObjectTypeGuid     = pcObjectTypeGuid
                                               AND SmartAttributeValue.ObjectMasterGuid   = "":U
                                               AND SmartAttributeValue.ObjectInstanceGuid = "":U
                                               AND SmartAttributeValue.AppliesAtRuntime   = TRUE
                                               AND LOOKUP (SmartAttributeValue.AttributeLabel, pcFieldNames) = 0
                                               AND SmartAttributeValue.AppliesAtRuntime = TRUE NO-LOCK,
                    FIRST SmartAttribute OF SmartAttributeValue NO-LOCK .
        ELSE /* DesignTime */
            OPEN QUERY qQuery
                FOR EACH SmartAttributeValue WHERE SmartAttributeValue.ObjectTypeGuid     = pcObjectTypeGuid
                                               AND SmartAttributeValue.ObjectMasterGuid   = "":U
                                               AND SmartAttributeValue.ObjectInstanceGuid = "":U
                                               AND SmartAttributeValue.AppliesAtRuntime   = TRUE
                                               AND LOOKUP (SmartAttributeValue.AttributeLabel, pcFieldNames) = 0 NO-LOCK,
                    FIRST SmartAttribute OF SmartAttributeValue WHERE SmartAttribute.RuntimeOnly = FALSE NO-LOCK .

        GET FIRST qQuery .

        DO WHILE NOT QUERY qQuery:QUERY-OFF-END:

            ASSIGN pcFieldNames = pcFieldNames + ",":U + SmartAttribute.AttributeLabel .

            CASE SmartAttribute.RepositoryType:
                WHEN "CHARACTER":U THEN
                    phTable:ADD-NEW-FIELD (SmartAttribute.AttributeLabel,
                                           SmartAttribute.RepositoryType,
                                           ?, /* extent */
                                           ?, /* format */
                                           SmartAttributeValue.CharacterValue) .
                WHEN "INTEGER":U THEN
                    phTable:ADD-NEW-FIELD (SmartAttribute.AttributeLabel,
                                           SmartAttribute.RepositoryType,
                                           ?, /* extent */
                                           ?, /* format */
                                           SmartAttributeValue.IntegerValue) .
                WHEN "INT64":U THEN
                    phTable:ADD-NEW-FIELD (SmartAttribute.AttributeLabel,
                                           SmartAttribute.RepositoryType,
                                           ?, /* extent */
                                           ?, /* format */
                                           SmartAttributeValue.Int64Value) .
                WHEN "DATE":U THEN
                    phTable:ADD-NEW-FIELD (SmartAttribute.AttributeLabel,
                                           SmartAttribute.RepositoryType,
                                           ?, /* extent */
                                           ?, /* format */
                                           SmartAttributeValue.DateValue) .
                WHEN "DATETIME":U THEN
                    phTable:ADD-NEW-FIELD (SmartAttribute.AttributeLabel,
                                           SmartAttribute.RepositoryType,
                                           ?, /* extent */
                                           ?, /* format */
                                           SmartAttributeValue.DateTimeValue) .
                WHEN "DATETIME-TZ":U THEN
                    phTable:ADD-NEW-FIELD (SmartAttribute.AttributeLabel,
                                           SmartAttribute.RepositoryType,
                                           ?, /* extent */
                                           ?, /* format */
                                           SmartAttributeValue.DateTimeTzValue) .
                WHEN "DECIMAL":U THEN
                    phTable:ADD-NEW-FIELD (SmartAttribute.AttributeLabel,
                                           SmartAttribute.RepositoryType,
                                           ?, /* extent */
                                           ?, /* format */
                                           SmartAttributeValue.DecimalValue) .
                WHEN "LOGICAL":U THEN
                    phTable:ADD-NEW-FIELD (SmartAttribute.AttributeLabel,
                                           SmartAttribute.RepositoryType,
                                           ?, /* extent */
                                           ?, /* format */
                                           SmartAttributeValue.LogicalValue) .
                WHEN "RAW":U THEN
                    phTable:ADD-NEW-FIELD (SmartAttribute.AttributeLabel,
                                           SmartAttribute.RepositoryType,
                                           ?, /* extent */
                                           ?, /* format */
                                           SmartAttributeValue.RawValue) .
                OTHERWISE
                    UNDO, THROW NEW InvalidRepositoryDataTypeException (pcObjectTypeGuid,
                                                                        SmartAttribute.AttributeLabel,
                                                                        SmartAttribute.RepositoryType) .
            END CASE .

            GET NEXT qQuery .
        END.

        ASSIGN pcFieldNames = TRIM (pcFieldNames, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the attribute cache temp-table for the given object type
        Notes:
        @param pcObjectType The object type to create the attribute cache temp-table for
        @param poAttributesFrom The GetAttributesForEnum indicating if attributes should be added for runtime or design
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateCacheForObjectType (pcObjectType AS CHARACTER,
                                                    poAttributesFrom AS GetAttributesForEnum):

        DEFINE VARIABLE oObjectType      AS ObjectTypeDatasetModel NO-UNDO .
        DEFINE VARIABLE oParentType      AS ObjectTypeDatasetModel NO-UNDO .
        DEFINE VARIABLE cParentType      AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE cParentTypeNames AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE cParentTypeGuids AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE cAllFieldNames   AS CHARACTER              NO-UNDO .

        DEFINE VARIABLE hTable           AS HANDLE                 NO-UNDO .

        DEFINE BUFFER ttAttributesCache FOR ttAttributesCache .

        oObjectType = NEW ObjectTypeDatasetModel (pcObjectType) .

        IF NOT oObjectType:SmartObjectType:Available THEN DO:
            oObjectType:SmartObjectType:Filter:Reset () .
            oObjectType:SmartObjectType:Filter:ObjectTypeName:EQ (pcObjectType):Run () .

            IF NOT oObjectType:SmartObjectType:Available THEN
                UNDO, THROW NEW InvalidObjectTypeException (pcObjectType) .
        END.

        CREATE TEMP-TABLE hTable .

        THIS-OBJECT:AddFieldsFromObjectType (oObjectType:SmartObjectType:ObjectTypeGuid,
                                             hTable,
                                             INPUT-OUTPUT cAllFieldNames,
                                             poAttributesFrom) .

        ASSIGN cParentTypeGuids = oObjectType:SmartObjectType:ObjectTypeGuid
               cParentTypeNames = oObjectType:SmartObjectType:ObjectTypeName .

        IF oObjectType:SmartObjectType:ExtendsObjectTypeGuid > "":U THEN DO:

            oParentType = NEW ObjectTypeDatasetModel (oObjectType:SmartObjectType:ExtendsObjectTypeGuid) .

            parentTypeLoop:
            DO WHILE oParentType:SmartObjectType:Available:

                ASSIGN cParentType = oParentType:SmartObjectType:ExtendsObjectTypeGuid .

                THIS-OBJECT:AddFieldsFromObjectType (oParentType:SmartObjectType:ObjectTypeGuid,
                                                     hTable,
                                                     INPUT-OUTPUT cAllFieldNames,
                                                     poAttributesFrom) .

                ASSIGN cParentTypeGuids = cParentTypeGuids + CHR(9) + oParentType:SmartObjectType:ObjectTypeGuid
                       cParentTypeNames = cParentTypeNames + CHR(9) + oParentType:SmartObjectType:ObjectTypeName .

                IF CharacterType:IsNullOrEmpty (cParentType) THEN
                    LEAVE parentTypeLoop .

                oParentType:SmartObjectType:Filter:Reset () .
                oParentType:SmartObjectType:Filter:ObjectTypeGuid:EQ (cParentType):Run ().
            END.
        END.

        hTable:ADD-NEW-FIELD ("_ParentTypeNames":U, "CHARACTER":U, ?, "x(73)":U, cParentTypeNames) .
        hTable:ADD-NEW-FIELD ("_ParentTypeGuids":U, "CHARACTER":U, ?, "x(73)":U, cParentTypeGuids) .
        hTable:ADD-NEW-FIELD ("_ObjectTypeGuid":U, "CHARACTER":U, ?, "x(36)":U, oObjectType:SmartObjectType:ObjectTypeGuid) .
        hTable:ADD-NEW-FIELD ("_ObjectMasterGuid":U, "CHARACTER":U, ?, "x(36)":U, ?) .
        hTable:ADD-NEW-FIELD ("_ObjectName":U, "CHARACTER":U, ?, "x(36)":U, ?) .
        hTable:ADD-NEW-FIELD ("_ObjectInstanceGuid":U, "CHARACTER":U, ?, "x(36)":U, ?) .
        hTable:ADD-NEW-FIELD ("_InstanceName":U, "CHARACTER":U, ?, "x(36)":U, ?) .
        hTable:ADD-NEW-FIELD ("_ExplicitAttributeValues":U, "CHARACTER":U, ?, "x(36)":U, "":U) .

        hTable:ADD-NEW-INDEX ("Instance":U, TRUE, TRUE, FALSE) .
        hTable:ADD-INDEX-FIELD ("Instance":U, "_ObjectInstanceGuid":U) .

        hTable:TEMP-TABLE-PREPARE (oObjectType:SmartObjectType:ObjectTypeName) .

        CREATE ttAttributesCache .
        ASSIGN ttAttributesCache.ClassName  = pcObjectType
               ttAttributesCache.CacheType  = poAttributesFrom:ToString()
               ttAttributesCache.Attributes = hTable .

        RELEASE ttAttributesCache .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a dataset handle that contains the cache temp-tables for the
                 given object types
        Notes:   The caller is responsible for deleting the returned Dataset
        @param poAttributesFrom The GetAttributesForEnum indicating if attributes should be added for runtime or design
        @param poParameter The CharacterList with the names or guids of the object types for this call
        @return The dataset handle that contains the cache temp-tables for the given object types
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetRepositoryCacheTables (poAttributesFrom AS GetAttributesForEnum,
                                                   poParameter AS CharacterList):

        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.

        CREATE DATASET hDataset .

        THIS-OBJECT:GetRepositoryCacheTables(poAttributesFrom, poParameter, hDataset) .

        RETURN hDataset .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a dataset handle that contains the cache temp-tables for the
                 given object types
        Notes:   The caller is responsible for deleting the returned Dataset
        @param poAttributesFrom The GetAttributesForEnum indicating if attributes should be added for runtime or design
        @param poParameter The CharacterList with the names or guids of the object types for this call
        @param phDataset The dataset handle to add the cache temp-tables for the given object types to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetRepositoryCacheTables (poAttributesFrom AS GetAttributesForEnum,
                                                 poParameter AS CharacterList,
                                                 phDataset AS HANDLE):

        DEFINE VARIABLE hTable   AS HANDLE                 NO-UNDO .

        {Consultingwerk/foreachPrimitiveList.i Character cObjectType in poParameter}

            FIND ttAttributesCache WHERE ttAttributesCache.CacheType = poAttributesFrom:ToString()
                                     AND ttAttributesCache.ClassName = cObjectType
                NO-ERROR .

            ErrorHelper:ResetErrorStatus() .

            IF lLogging THEN
                LogManager:WriteFormattedMessage ("[GetRepositoryCacheTables] &1 / &2 - in Cache: &3":U,
                                                  cObjectType,
                                                  poAttributesFrom:ToString(),
                                                  STRING (AVAILABLE ttAttributesCache),
                                                  NEW MessageSubSystem("SmartRepositoryService":U)) .

            IF NOT AVAILABLE ttAttributesCache THEN DO:
                THIS-OBJECT:CreateCacheForObjectType (cObjectType, poAttributesFrom) .

                FIND ttAttributesCache WHERE ttAttributesCache.CacheType = poAttributesFrom:ToString()
                                         AND ttAttributesCache.ClassName = cObjectType .
            END.

            CREATE TEMP-TABLE hTable.
            hTable:CREATE-LIKE (ttAttributesCache.Attributes) .
            hTable:TEMP-TABLE-PREPARE (ttAttributesCache.Attributes:NAME) .

            phDataset:ADD-BUFFER (hTable:DEFAULT-BUFFER-HANDLE) .
        END.

        FINALLY:
            FOR EACH ttAttributesCache WHERE ttAttributesCache.CacheType = GetAttributesForEnum:DesignTime:ToString()
                ON ERROR UNDO, THROW:

                IF VALID-HANDLE (ttAttributesCache.Attributes) THEN
                    DELETE OBJECT ttAttributesCache.Attributes .

                DELETE ttAttributesCache .
            END.
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the RepositoryCacheTableFactory class
        Notes:   Deletes cache temp-tables
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC RepositoryCacheTableFactory ():

        /* Delete chache temp-tables */
        FOR EACH ttAttributesCache ON ERROR UNDO, THROW:

            IF VALID-HANDLE (ttAttributesCache.Attributes) THEN
                DELETE OBJECT ttAttributesCache.Attributes .

            DELETE ttAttributesCache.
        END.

    END DESTRUCTOR.

END CLASS.
