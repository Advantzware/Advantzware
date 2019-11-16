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
    File        : SmartRepositoryService
    Purpose     : Provides access to repository
                  data on the client and appserver
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 15 22:29:57 CEST 2016
    Notes       : Supports the SmartRepositoryService custom log entry type
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                  FROM PROPATH .
USING Consultingwerk.Assertion.*                        FROM PROPATH .
USING Consultingwerk.Exceptions.*                       FROM PROPATH .
USING Consultingwerk.Framework.*                        FROM PROPATH .
USING Consultingwerk.Framework.Collections.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.*                   FROM PROPATH .
USING Consultingwerk.OERA.*                             FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*              FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.*  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Field.*  FROM PROPATH.
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*            FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Consultingwerk.Web2.Services.Rendering.*          FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*         FROM PROPATH.
USING Progress.Lang.*                                   FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Repository.SmartRepositoryService
    IMPLEMENTS ISmartRepositoryService:

    DEFINE TEMP-TABLE ttClassIsA NO-UNDO
        FIELD ChildClassName  AS CHARACTER
        FIELD ParentClassName AS CHARACTER
        FIELD IsA             AS LOGICAL
        INDEX ClassNames IS UNIQUE ChildClassName ParentClassName .

    DEFINE TEMP-TABLE ttObjectIsA NO-UNDO
        FIELD ObjectName  AS CHARACTER
        FIELD ClassName   AS CHARACTER
        FIELD IsA         AS LOGICAL
        INDEX ClassNames IS UNIQUE ObjectName ClassName .

    DEFINE TEMP-TABLE ttChildClasses NO-UNDO
        FIELD ParentClassNames AS CHARACTER
        FIELD ChildClassNames AS CHARACTER
        INDEX ParentClassNames IS UNIQUE ParentClassNames .

    DEFINE VARIABLE lLogging AS LOGICAL NO-UNDO INITIAL FALSE .

    /**
     * Purpose: Constructor for the SmartRepositoryService class
     * Notes:
     */
    CONSTRUCTOR PUBLIC SmartRepositoryService():

        ASSIGN lLogging = LogManager:IsCustomLogEntryTypeActive ("SmartRepositoryService":U) .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns if the child class is a child of the given parent class
     * Notes:
     * @param pcChildClassName The name of the child class to verify
     * @param pcParentClassName The name of the parent class to test if the given child class is a child of
     * @return Logical value indicating if the child class is child of the given parent class
     */
    METHOD PUBLIC LOGICAL ClassIsA (pcChildClassName AS CHARACTER,
                                    pcParentClassName AS CHARACTER):

        DEFINE VARIABLE oParameter AS ClassIsAParameter NO-UNDO .
        DEFINE VARIABLE hDataset   AS HANDLE            NO-UNDO .

        IF CAN-FIND (ttClassIsA WHERE ttClassIsA.ChildClassName  = pcChildClassName
                                  AND ttClassIsA.ParentClassName = pcParentClassName) THEN DO:

            FIND ttClassIsA WHERE ttClassIsA.ChildClassName  = pcChildClassName
                              AND ttClassIsA.ParentClassName = pcParentClassName NO-ERROR .

            RETURN ttClassIsA.IsA .
        END.

        ASSIGN oParameter = NEW ClassIsAParameter(pcChildClassName, pcParentClassName) .

        IF CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U,
                                           "ClassIsA":U,
                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                           oParameter) .
        ELSE
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U,
                                                           "ClassIsA":U,
                                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                           oParameter) .

        CREATE ttClassIsA .
        ASSIGN ttClassIsA.ChildClassName  = pcChildClassName
               ttClassIsA.ParentClassName = pcParentClassName
               ttClassIsA.IsA             = oParameter:IsA .

        RELEASE ttClassIsA .

        RETURN oParameter:IsA .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Creates MetaEntityTable, MetaDataField and MetaCalculatedField
     *          object masters for the given Business Entity Model
     * Notes:   When the poOverwritePromptCallback reference is not valid, we'll
     *          overwrite existing objects silently
     * @param phBusinessEntityDataset The ProDataset with the Business Entity Model to import
     * @param pcModuleName The name of the Product Module to import the entity objects into
     * @param poOverwritePromptCallback The callback reference to prompt the user if an existing object should be overwritten
     * @return The CharacterList with the GUID's of the created MetaEntityTable objects
     */
    METHOD PUBLIC CharacterList CreateDataFields (phBusinessEntityDataset AS HANDLE,
                                                  pcModuleName AS CHARACTER,
                                                  poOverwritePromptCallback AS IOverwritePromptCallback):

        THIS-OBJECT:CreateDataFields (phBusinessEntityDataset, pcModuleName, poOverwritePromptCallback, ?, ?, ?) .

    END METHOD .

    /**
     * Purpose: Creates MetaEntityTable, MetaDataField and MetaCalculatedField
     *          object masters for the given Business Entity Model
     * Notes:   When the poOverwritePromptCallback reference is not valid, we'll
     *          overwrite existing objects silently
     * @param phBusinessEntityDataset The ProDataset with the Business Entity Model to import
     * @param pcModuleName The name of the Product Module to import the entity objects into
     * @param poOverwritePromptCallback The callback reference to prompt the user if an existing object should be overwritten
     * @param pcEntityTableType The type to use for table objects
     * @param pcDataFieldType The type to use for data field objects
     * @param pcCalculatedFieldType The type to use for calculated field objects
     * @return The CharacterList with the GUID's of the created MetaEntityTable objects
     */
    METHOD PUBLIC CharacterList CreateDataFields (phBusinessEntityDataset AS HANDLE,
                                                  pcModuleName AS CHARACTER,
                                                  poOverwritePromptCallback AS IOverwritePromptCallback,
                                                  pcEntityTableType AS CHARACTER,
                                                  pcDataFieldType AS CHARACTER,
                                                  pcCalculatedFieldType AS CHARACTER):

        DEFINE VARIABLE hTableQuery                    AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hTable                         AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hFieldQuery                    AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hField                         AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE oTable                         AS ObjectMasterDatasetModel     NO-UNDO .
        DEFINE VARIABLE oField                         AS ObjectMasterDatasetModel     NO-UNDO .
        DEFINE VARIABLE cFieldName                     AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cFieldInstanceGuid             AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE oInstance                      AS ObjectMasterDatasetModel     NO-UNDO .
        DEFINE VARIABLE iExtent                        AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE lImportAttributes              AS LOGICAL                      NO-UNDO .
        DEFINE VARIABLE hAttributes                    AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hAttributeBuffer               AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE oCacheFactory                  AS IRepositoryCacheTableFactory NO-UNDO .

        DEFINE VARIABLE cEntityTableTypeGuid           AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cEntityDataFieldTypeGuid       AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cEntityCalculatedFieldTypeGuid AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cModuleGuid                    AS CHARACTER                    NO-UNDO .

        DEFINE VARIABLE oResult                        AS CharacterList                NO-UNDO .

        HandleAssert:IsDataset (phBusinessEntityDataset) .

        IF CharacterType:IsNullOrEmpty (pcEntityTableType) THEN
            ASSIGN pcEntityTableType = "MetaEntityTable":U .

        IF CharacterType:IsNullOrEmpty (pcDataFieldType) THEN
            ASSIGN pcDataFieldType = "MetaDataField":U .

        IF CharacterType:IsNullOrEmpty (pcCalculatedFieldType) THEN
            ASSIGN pcCalculatedFieldType = "MetaCalculatedField":U .

        ASSIGN cEntityTableTypeGuid           = THIS-OBJECT:GetObjectTypeGuid (pcEntityTableType)
               cEntityDataFieldTypeGuid       = THIS-OBJECT:GetObjectTypeGuid (pcDataFieldType)
               cEntityCalculatedFieldTypeGuid = THIS-OBJECT:GetObjectTypeGuid (pcCalculatedFieldType)
               cModuleGuid                    = THIS-OBJECT:GetProductModuleGuid (pcModuleName).

        oTable    = NEW ObjectMasterDatasetModel() .
        oField    = NEW ObjectMasterDatasetModel() .
        oInstance = NEW ObjectMasterDatasetModel() .
        oResult   = NEW CharacterList () .

        oCacheFactory = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.Class.IRepositoryCacheTableFactory
                                                      "NEW RepositoryCacheTableFactory()"} .

        ASSIGN hTable      = phBusinessEntityDataset::eTable
               hField      = phBusinessEntityDataset::eField
               hTableQuery = QueryHelper:CreatePreparedQuery (hTable).

        DO WHILE NOT hTableQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

            oTable:SmartObjectMaster:Filter:Reset() .
            oTable:SmartObjectMaster:Filter:ObjectName:EQ (hTable::TempTableName):Run () .

            IF oTable:SmartObjectMaster:Available = FALSE THEN DO:

                oTable:TrackingChanges = TRUE .
                oTable:SmartObjectMaster:Create() .

                ASSIGN
                  oTable:SmartObjectMaster:ObjectName        = hTable::TempTableName
                  oTable:SmartObjectMaster:ObjectTypeGuid    = cEntityTableTypeGuid
                  oTable:SmartObjectMaster:ModuleGuid        = cModuleGuid
                  oTable:SmartObjectMaster:ObjectDescription = StringHelper:FirstNonEmptyString (hTable::TempTablePurpose,
                                                                                                 hTable::TempTableDescription,
                                                                                                 SUBSTITUTE ("Business Entity Table &1":U, hTable::TempTableName)) .
                oTable:SaveChanges() .
            END.
            ELSE IF NOT VALID-OBJECT (poOverwritePromptCallback) OR poOverwritePromptCallback:PromptForOverwrite (hTable::TempTablePurpose) THEN DO:
                ASSIGN
                  oTable:SmartObjectMaster:ObjectDescription = StringHelper:FirstNonEmptyString (hTable::TempTablePurpose,
                                                                                                 hTable::TempTableDescription,
                                                                                                 SUBSTITUTE ("Business Entity Table &1":U, hTable::TempTableName)) .
                oTable:SaveChanges() .
            END.

            oResult:Add (oTable:SmartObjectMaster:ObjectMasterGuid) .

            ASSIGN hFieldQuery = QueryHelper:CreatePreparedQuery (hField,
                                                                  SUBSTITUTE ("for each eField where eField.TempTableName = &1":U,
                                                                              QUOTER (hTable::TempTableName))) .

            DO WHILE NOT hFieldQuery:QUERY-OFF-END:

                DO iExtent = 1 TO MAXIMUM (1, hField::FieldExtent):

                    IF hField::FieldExtent = 0 OR hField::FieldExtent = ? THEN
                        ASSIGN cFieldName = SUBSTITUTE ("&1.&2":U, hField::TempTableName, hField::FieldName) .
                    ELSE
                        ASSIGN cFieldName = SUBSTITUTE ("&1.&2[&3]":U, hField::TempTableName, hField::FieldName, iExtent) .

                    oField:SmartObjectMaster:Filter:Reset() .
                    oField:SmartObjectMaster:Filter:ObjectName:EQ (cFieldName):Run () .

                    IF oField:SmartObjectMaster:Available = FALSE THEN DO:

                        oField:TrackingChanges = TRUE .
                        oField:SmartObjectMaster:Create() .

                        ASSIGN
                          oField:SmartObjectMaster:ObjectName        = cFieldName
                          oField:SmartObjectMaster:ObjectTypeGuid    = IF hField::FieldSource > "":U THEN cEntityDataFieldTypeGuid ELSE cEntityCalculatedFieldTypeGuid
                          oField:SmartObjectMaster:ModuleGuid        = cModuleGuid
                          oField:SmartObjectMaster:ObjectDescription = StringHelper:FirstNonEmptyString (hField::FieldDescription,
                                                                                                         SUBSTITUTE ("Business Entity Field &1":U, cFieldName)) .
                        oField:SaveChanges() .
                    END.
                    ELSE IF NOT VALID-OBJECT (poOverwritePromptCallback) OR poOverwritePromptCallback:PromptForOverwrite (cFieldName) THEN DO:
                        ASSIGN
                          oField:SmartObjectMaster:ObjectDescription = StringHelper:FirstNonEmptyString (hField::FieldDescription,
                                                                                                         SUBSTITUTE ("Business Entity Field &1":U, cFieldName)) .
                        oField:SaveChanges() .
                    END.

                    oInstance:SmartObjectInstance:Filter:Reset () .
                    oInstance:SmartObjectInstance:Filter:ContainerObjectMasterGuid:EQ (oTable:SmartObjectMaster:ObjectMasterGuid) .
                    IF NOT oInstance:SmartObjectInstance:Filter:InstanceName:EQ (cFieldName):CanFind () THEN DO:
                        ASSIGN
                            lImportAttributes = TRUE
                            cFieldInstanceGuid = THIS-OBJECT:CreateInstance (oTable:SmartObjectMaster:ObjectMasterGuid,
                                                                             oField:SmartObjectMaster:ObjectMasterGuid,
                                                                             cFieldName,
                                                                             "",
                                                                             0,
                                                                             hField::FieldOrder) .
                    END.
                    ELSE DO:
                        oInstance:SmartObjectInstance:Filter:Reset () .
                        oInstance:SmartObjectInstance:Filter:ContainerObjectMasterGuid:EQ (oTable:SmartObjectMaster:ObjectMasterGuid) .
                        oInstance:SmartObjectInstance:Filter:InstanceName:EQ (cFieldName):Run () .

                        ASSIGN
                            lImportAttributes  = NOT VALID-OBJECT (poOverwritePromptCallback) OR poOverwritePromptCallback:PromptForOverwrite (cFieldName)
                            cFieldInstanceGuid = oInstance:SmartObjectInstance:ObjectInstanceGuid .
                    END.

                    IF lImportAttributes THEN DO ON ERROR UNDO, THROW:

                        hAttributes = oCacheFactory:GetRepositoryCacheTables (GetAttributesForEnum:DesignTime,
                                                                              NEW CharacterList (oField:SmartObjectMaster:ObjectTypeGuid)) .
                        hAttributeBuffer = hAttributes:GET-BUFFER-HANDLE(1) .

                        hAttributeBuffer:BUFFER-CREATE () .

                        ASSIGN hAttributeBuffer::_ObjectMasterGuid = oField:SmartObjectMaster:ObjectMasterGuid .

                        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
                                                       "FetchObjectMasterAttributes":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hAttributes BY-REFERENCE, ?) .

                        hAttributeBuffer:FIND-FIRST () .

                        ASSIGN
                            hAttributeBuffer::DATA-TYPE                             = hField::FieldDataType
                            hAttributeBuffer::InitialValue                          = IF hField::FieldInitialUnknownValue THEN ? ELSE hField::FieldInitial
                            hAttributeBuffer:BUFFER-FIELD ("LABEL":U):BUFFER-VALUE  = hField::FieldLabel
                            hAttributeBuffer:BUFFER-FIELD ("FORMAT":U):BUFFER-VALUE = hField::FieldFormat
                            .

                        IF hField::FieldFormat > "":U THEN
                            hAttributeBuffer::WIDTH-PIXELS = FormatHelper:GetFormatWidthPixels(hField::FieldFormat) .

                        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
                                                       "StoreObjectMasterAttributes":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hAttributes BY-REFERENCE, ?) .

                        FINALLY:
                            DELETE OBJECT hAttributes .
                        END FINALLY.
                    END.

                END. /* iExtent */

                hFieldQuery:GET-NEXT () .
            END.

            hTableQuery:GET-NEXT () .

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hFieldQuery) .
            END FINALLY.
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hTableQuery) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Creates an Object Instance
     * Notes:
     * @param pcContainerObject The Container Object Master GUID or Container Object Name
     * @param pcInstanceObject The Object Master GUID or Object Name of the object to create an instance of
     * @param pcInstanceName The name of the object instance (or ? to use the object name)
     * @param pcInstanceDescription The description of the instance
     * @param piPageSequence The sequence number of the page to create the instance on, zero for container page
     * @param piObjectSequence The sequence of the object instance on the given page
     * @return The Object Instance Guid
     */
    METHOD PUBLIC CHARACTER CreateInstance (pcContainerObject AS CHARACTER,
                                            pcInstanceObject AS CHARACTER,
                                            pcInstanceName AS CHARACTER,
                                            pcInstanceDescription AS CHARACTER,
                                            piPageSequence AS INTEGER,
                                            piObjectSequence AS INTEGER):

        DEFINE VARIABLE cPage AS CHARACTER NO-UNDO.

        IF piPageSequence = ? THEN
            ASSIGN cPage = "":U .
        ELSE
            ASSIGN cPage = STRING (piPageSequence) .

        RETURN THIS-OBJECT:CreateInstance (pcContainerObject,
                                           pcInstanceObject,
                                           pcInstanceName,
                                           pcInstanceDescription,
                                           cPage,
                                           piObjectSequence) .

    END METHOD .

    /**
     * Purpose: Creates an Object Instance
     * Notes:
     * @param pcContainerObject The Container Object Master GUID or Container Object Name
     * @param pcInstanceObject The Object Master GUID or Object Name of the object to create an instance of
     * @param pcInstanceName The name of the object instance (or ? to use the object name)
     * @param pcInstanceDescription The description of the instance
     * @param pcPageGuid The GUID or the sequence number of the page to create the instance on, leave empty for container page
     * @param piObjectSequence The sequence of the object instance on the given page
     * @return The Object Instance Guid
     */
    METHOD PUBLIC CHARACTER CreateInstance (pcContainerObject AS CHARACTER,
                                            pcInstanceObject AS CHARACTER,
                                            pcInstanceName AS CHARACTER,
                                            pcInstanceDescription AS CHARACTER,
                                            pcPageGuid AS CHARACTER,
                                            piObjectSequence AS INTEGER):

        DEFINE VARIABLE oPage     AS ObjectMasterDatasetModel NO-UNDO .
        DEFINE VARIABLE oInstance AS ObjectMasterDatasetModel NO-UNDO .
        DEFINE VARIABLE iPage     AS INTEGER                  NO-UNDO .

        ASSIGN
            pcContainerObject = THIS-OBJECT:GetObjectMasterGuid (pcContainerObject)
            pcInstanceObject  = THIS-OBJECT:GetObjectMasterGuid (pcInstanceObject) .

        IF CharacterType:IsNullOrEmpty (pcInstanceName) THEN
            ASSIGN pcInstanceName = NEW ObjectMasterDatasetModel (pcInstanceObject):SmartObjectMaster:ObjectName .

        IF pcPageGuid = "0":U THEN
            ASSIGN pcPageGuid = "":U .

        IF pcPageGuid > "":U THEN DO:
            oPage = NEW ObjectMasterDatasetModel() .
            oPage:SmartPage:Filter:ContainerObjectMasterGuid:EQ (pcContainerObject) .

            IF NOT oPage:SmartPage:Filter:PageGuid:EQ (pcPageGuid):CanFind () THEN DO:
                ASSIGN iPage = DataTypeHelper:ToInteger (pcPageGuid) .

                IF iPage = ? OR iPage = 0 THEN
                    UNDO, THROW NEW InvalidParameterValueException ("pcPageGuid":U, pcPageGuid, THIS-OBJECT:GetClass():TypeName) .

                oPage:SmartPage:Filter:Reset() .
                oPage:SmartPage:Filter:ContainerObjectMasterGuid:EQ (pcContainerObject) .
                oPage:SmartPage:Filter:PageSequence:EQ (iPage):Run () .

                IF oPage:SmartPage:Available THEN
                    ASSIGN pcPageGuid = oPage:SmartPage:PageGuid .
                ELSE
                    UNDO, THROW NEW InvalidParameterValueException ("pcPageGuid":U, pcPageGuid, THIS-OBJECT:GetClass():TypeName) .
            END.
        END .

        oInstance = NEW ObjectMasterDatasetModel().
        oInstance:TrackingChanges = TRUE .
        oInstance:SmartObjectInstance:Create() .

        ASSIGN
            oInstance:SmartObjectInstance:ContainerObjectMasterGuid = pcContainerObject
            oInstance:SmartObjectInstance:ObjectMasterGuid          = pcInstanceObject
            oInstance:SmartObjectInstance:InstanceName              = pcInstanceName
            oInstance:SmartObjectInstance:InstanceDescription       = pcInstanceDescription
            oInstance:SmartObjectInstance:PageGuid                  = pcPageGuid
            oInstance:SmartObjectInstance:ObjectSequence            = piObjectSequence .

        oInstance:SaveChanges() .

        RETURN oInstance:SmartObjectInstance:ObjectInstanceGuid .

    END METHOD .

    /**
     * Purpose: Clears all caches of this class
     * Notes:
     */
    METHOD PUBLIC VOID ClearCaches():

        EMPTY TEMP-TABLE ttClassIsA .
        EMPTY TEMP-TABLE ttObjectIsA .
        EMPTY TEMP-TABLE ttChildClasses .

    END METHOD .

    /**
     * Purpose: Ensures that the list of classes contains GUIDs and not class names
     * Notes:   Turns Class names into guids
     * @param pcClassNames The comma delimited list of class names or class guids
     * @return The comma delimited list of class guids
     */
    METHOD PUBLIC CHARACTER EnsureGuids (pcClassNames AS CHARACTER):

        DEFINE VARIABLE i           AS INTEGER                NO-UNDO .
        DEFINE VARIABLE iCount      AS INTEGER                NO-UNDO .
        DEFINE VARIABLE cEntry      AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE oObjectType AS ObjectTypeDatasetModel NO-UNDO .

        ASSIGN iCount      = NUM-ENTRIES (pcClassNames)
               oObjectType = NEW ObjectTypeDatasetModel ()
               oObjectType:UseDedicatedBusinessEntity = TRUE .

        classLoop:
        DO i = 1 TO iCount:

            ASSIGN cEntry = ENTRY (i, pcClassNames) .

            oObjectType:SmartObjectType:Filter:Reset() .
            IF oObjectType:SmartObjectType:Filter:ObjectTypeGuid:EQ (cEntry):CanFind() THEN
                NEXT classLoop .

            oObjectType:SmartObjectType:Filter:Reset() .
            oObjectType:SmartObjectType:Filter:ObjectTypeName:EQ (cEntry):Run () .

            IF oObjectType:SmartObjectType:Available = FALSE THEN
                UNDO, THROW NEW RepositoryException (SUBSTITUTE ("Unknown object type: &1"{&TRAN}, cEntry), 0) .

            ENTRY (i, pcClassNames) = oObjectType:SmartObjectType:ObjectTypeGuid .
        END.

        RETURN pcClassNames .

        FINALLY:
            GarbageCollectorHelper:DeleteObject(oObjectType) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Fetches an object from the repository
     * Notes:
     * @param poParameter The FetchRepositoryObjectParameter with the parameter for this call
     * @return The handle of the dataset with the repository data
     */
    METHOD PUBLIC HANDLE FetchRepositoryObject (poParameter AS FetchRepositoryObjectParameter):

        DEFINE VARIABLE hDataset                      AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hInstancesDataset             AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hNestedDataset                AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hNestedBuffer                 AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hNewTable                     AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE iNestedBuffer                 AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE oRequest                      AS IFetchDataRequest            NO-UNDO .
        DEFINE VARIABLE cQueryString                  AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE hQuery                        AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE oTypes                        AS CharacterList                NO-UNDO .
        DEFINE VARIABLE oCacheTableFactory            AS IRepositoryCacheTableFactory NO-UNDO .
        DEFINE VARIABLE hBuffer                       AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE oAttributeValueBusinessEntity AS AttributeValueBusinessEntity NO-UNDO .
        DEFINE VARIABLE hInstanceTypeMap              AS HANDLE                       NO-UNDO .

        ObjectAssert:IsValid(poParameter, "poParameter":U) .

        /* Fetch direct container data */
        oRequest = NEW FetchDataRequest ("eSmartObjectMaster,eSmartObjectInstance,eSmartLink,eSmartPage":U,
                                         SUBSTITUTE ("for each eSmartObjectMaster where eSmartObjectMaster.ObjectName = &1":U, QUOTER (poParameter:ContainerName)),
                                         0) .
        oRequest:CustomContext = "SkipCalculatedFields":U .

        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U,
                                    oRequest,
                                    OUTPUT DATASET-HANDLE hDataset) .

        hDataset::eSmartObjectMaster:FIND-FIRST () NO-ERROR .

        IF NOT hDataset::eSmartObjectMaster:AVAILABLE THEN DO:

            /* Fetch direct container data */
            oRequest:Queries = SUBSTITUTE ("for each eSmartObjectMaster where eSmartObjectMaster.ObjectMasterGuid = &1":U,
                                           QUOTER (poParameter:ContainerName)).

            ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U,
                                        oRequest,
                                        OUTPUT DATASET-HANDLE hDataset) .

            hDataset::eSmartObjectMaster:FIND-FIRST () NO-ERROR .
        END.

        IF NOT hDataset::eSmartObjectMaster:AVAILABLE THEN
            UNDO, THROW NEW InvalidObjectMasterException (SUBSTITUTE ("Invalid container name: &1"{&TRAN}, poParameter:ContainerName),
                                                          0,
                                                          poParameter:ContainerName) .

        CREATE TEMP-TABLE hInstanceTypeMap .
        hInstanceTypeMap:ADD-NEW-FIELD ("ObjectInstanceGuid":U, "CHARACTER":U) .
        hInstanceTypeMap:ADD-NEW-FIELD ("ObjectTypeName":U, "CHARACTER":U) .
        hInstanceTypeMap:ADD-NEW-INDEX ("ObjectInstanceGuid":U, TRUE, TRUE, FALSE) .
        hInstanceTypeMap:ADD-INDEX-FIELD ("ObjectInstanceGuid":U, "ObjectInstanceGuid":U) .
        hInstanceTypeMap:TEMP-TABLE-PREPARE ("_InstanceTypeMap":U) .

        hDataset:ADD-BUFFER (hInstanceTypeMap:DEFAULT-BUFFER-HANDLE) .

        oTypes = NEW CharacterList () .
        oTypes:Add (hDataset::eSmartObjectMaster::ObjectTypeGuid) .

        /* Fetch object master records of instances */
        hDataset::eSmartObjectInstance:FIND-FIRST () NO-ERROR .

        IF hDataset::eSmartObjectInstance:AVAILABLE THEN DO ON ERROR UNDO, THROW:
            ASSIGN cQueryString = "":U
                   hQuery       = QueryHelper:CreatePreparedQuery (hDataset::eSmartObjectInstance).

            DO WHILE NOT hQuery:QUERY-OFF-END:
                ASSIGN cQueryString = cQueryString +
                                      (IF cQueryString > "":U THEN " OR ":U ELSE "":U) +
                                      "eSmartObjectMaster.ObjectMasterGuid = ":U +
                                      QUOTER (hDataset::eSmartObjectInstance::ObjectMasterGuid) .

                hQuery:GET-NEXT () .
            END.

            GarbageCollectorHelper:DeleteObject (hQuery) .

            ASSIGN oRequest:Tables     = "eSmartObjectMaster":U
                   oRequest:Queries    = "for each eSmartObjectMaster where ":U + cQueryString .


            ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U,
                                        oRequest,
                                        OUTPUT DATASET-HANDLE hInstancesDataset) .

            ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hInstancesDataset::eSmartObjectMaster) .

            DO WHILE NOT hQuery:QUERY-OFF-END:
                oTypes:AddWhenNotContained (hInstancesDataset::eSmartObjectMaster::ObjectTypeGuid) .

                hQuery:GET-NEXT () .
            END.

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hQuery) .
            END FINALLY.
        END .

        ASSIGN oCacheTableFactory = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.Class.IRepositoryCacheTableFactory
                                                                  "NEW Consultingwerk.SmartFramework.Repository.Class.RepositoryCacheTableFactory ()"}
               oAttributeValueBusinessEntity = CAST (ServiceManager:GetBusinessService ("Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U),
                                                     AttributeValueBusinessEntity) .

        oCacheTableFactory:GetRepositoryCacheTables(GetAttributesForEnum:Runtime, oTypes, hDataset) .

        ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (STRING (hDataset::eSmartObjectMaster::ObjectTypeName)) .

        hBuffer:BUFFER-CREATE () .

        ASSIGN hBuffer::_ObjectMasterGuid = hDataset::eSmartObjectMaster::ObjectMasterGuid
               hBuffer::_ObjectName       = hDataset::eSmartObjectMaster::ObjectName .

        oAttributeValueBusinessEntity:FetchObjectMasterAttributes (INPUT-OUTPUT DATASET-HANDLE hDataset BY-REFERENCE, ?).

        hQuery = QueryHelper:CreatePreparedQuery (hDataset::eSmartObjectInstance).

        DO WHILE NOT hQuery:QUERY-OFF-END:

            hInstancesDataset::eSmartObjectMaster:FIND-FIRST (SUBSTITUTE ("where ObjectMasterGuid = &1":U, QUOTER (hDataset::eSmartObjectInstance::ObjectMasterGuid))) .

            hInstanceTypeMap:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE ().
            hInstanceTypeMap:DEFAULT-BUFFER-HANDLE::ObjectInstanceGuid = hDataset::eSmartObjectInstance::ObjectInstanceGuid .
            hInstanceTypeMap:DEFAULT-BUFFER-HANDLE::ObjectTypeName = hInstancesDataset::eSmartObjectMaster::ObjectTypeName .
            hInstanceTypeMap:DEFAULT-BUFFER-HANDLE:BUFFER-RELEASE () .

            hBuffer = hDataset:GET-BUFFER-HANDLE (STRING (hInstancesDataset::eSmartObjectMaster::ObjectTypeName)) .

            hBuffer:BUFFER-CREATE () .

            ASSIGN hBuffer::_ObjectMasterGuid   = hDataset::eSmartObjectInstance::ObjectMasterGuid
                   hBuffer::_ObjectName         = hDataset::eSmartObjectInstance::ObjectMasterName
                   hBuffer::_ObjectInstanceGuid = hDataset::eSmartObjectInstance::ObjectInstanceGuid
                   hBuffer::_InstanceName       = hDataset::eSmartObjectInstance::InstanceName .

            hQuery:GET-NEXT () .
        END.

        GarbageCollectorHelper:DeleteObject (hQuery) .

        oAttributeValueBusinessEntity:FetchObjectInstanceAttributes (INPUT-OUTPUT DATASET-HANDLE hDataset BY-REFERENCE, ?).

        /* Fetch nested instances */
        IF poParameter:FetchChildDetails THEN DO:

            hQuery = QueryHelper:CreatePreparedQuery (hDataset::eSmartObjectInstance, "preselect each eSmartObjectInstance":U).

            DO WHILE NOT hQuery:QUERY-OFF-END:

                hInstancesDataset::eSmartObjectMaster:FIND-FIRST (SUBSTITUTE ("where ObjectMasterGuid = &1":U, QUOTER (hDataset::eSmartObjectInstance::ObjectMasterGuid))) NO-ERROR .

                IF hInstancesDataset::eSmartObjectMaster::ContainerType = TRUE THEN DO ON ERROR UNDO, THROW:

                    hNestedDataset = THIS-OBJECT:FetchRepositoryObject(NEW FetchRepositoryObjectParameter (hInstancesDataset::eSmartObjectMaster::ObjectName,
                                                                                                           TRUE)) .

                    nestedBufferLoop:
                    DO iNestedBuffer = 1 TO hNestedDataset:NUM-BUFFERS:
                        ASSIGN hNestedBuffer = hNestedDataset:GET-BUFFER-HANDLE (iNestedBuffer) .

                        IF hNestedBuffer:NAME = "eSmartObjectMaster":U THEN
                            NEXT nestedBufferLoop .

                        IF DatasetHelper:HasBuffer(hDataset, hNestedBuffer:NAME) THEN
                            hDataset:GET-BUFFER-HANDLE (hNestedBuffer:NAME):TABLE-HANDLE:COPY-TEMP-TABLE (hNestedBuffer:TABLE-HANDLE, TRUE) .
                        ELSE DO:
                            CREATE TEMP-TABLE hNewTable .
                            hNewTable:CREATE-LIKE (hNestedBuffer) .
                            hNewTable:TEMP-TABLE-PREPARE (hNestedBuffer:NAME) .
                            hNewTable:COPY-TEMP-TABLE (hNestedBuffer:TABLE-HANDLE, TRUE) .
                            hDataset:ADD-BUFFER (hNewTable:DEFAULT-BUFFER-HANDLE) .
                        END.
                    END.

                    FINALLY:
                        DELETE OBJECT hNestedDataset .
                    END FINALLY.
                END.

                hQuery:GET-NEXT () .
            END.
        END.

        GarbageCollectorHelper:DeleteObject (hQuery) .

        RETURN hDataset .

        FINALLY:
            IF VALID-HANDLE (hInstancesDataset) THEN
                DELETE OBJECT hInstancesDataset .

            GarbageCollectorHelper:DeleteObject (hQuery) .

            ErrorHelper:ResetErrorStatus ().
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Returns a buffer to the attribute of a given object master (when
     *          pcInstanceName is empty) or named object instance
     * Notes:   This instance creates a new (dynamic) buffer. The caller is
     *          responsible for deleting the buffer
     * @param pcInstanceName The name of the instance to return the attribute buffer for, or "" or ? to return the attributes of the master
     * @param phRepositoryData The handle of the repository data ProDataset
     * @return The handle of the attribute buffer
     */
    METHOD PUBLIC HANDLE GetAttributeBuffer (pcInstanceName AS CHARACTER,
                                             phRepositoryData AS HANDLE):

        DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cTable  AS CHARACTER NO-UNDO.

        phRepositoryData::eSmartObjectMaster:FIND-FIRST () .

        IF pcInstanceName > "":U THEN DO:
            phRepositoryData::eSmartObjectInstance:FIND-UNIQUE (SUBSTITUTE ("where ContainerObjectMasterGuid = &1 and InstanceName = &2":U,
                                                                            QUOTER (phRepositoryData::eSmartObjectMaster::ObjectMasterGuid),
                                                                            QUOTER (pcInstanceName))) .

            phRepositoryData::_InstanceTypeMap:FIND-UNIQUE (SUBSTITUTE ("where ObjectInstanceGuid = &1":U,
                                                                        QUOTER (phRepositoryData::eSmartObjectInstance::ObjectInstanceGuid))) .

            cTable = phRepositoryData::_InstanceTypeMap::ObjectTypeName .

            CREATE BUFFER hBuffer FOR TABLE phRepositoryData:GET-BUFFER-HANDLE (cTable) .

            hBuffer:FIND-UNIQUE (SUBSTITUTE ("where _ObjectInstanceGuid = &1":U,
                                             QUOTER (phRepositoryData::eSmartObjectInstance::ObjectInstanceGuid))).
        END.
        ELSE DO:
            cTable = phRepositoryData::eSmartObjectMaster::ObjectTypeName .

            CREATE BUFFER hBuffer FOR TABLE phRepositoryData:GET-BUFFER-HANDLE (cTable) .

            hBuffer:FIND-UNIQUE ("where _ObjectInstanceGuid = ?":U) .
        END.

        RETURN hBuffer .

    END METHOD .

    /**
     * Purpose: Returns the comma delimited list of child class guids of the provided
     *          parent classes
     * Notes:
     * @param pcObjectTypeGuids The list of parent classes
     * @return The list of child class guids (including the parent classes)
     */
    METHOD PUBLIC CHARACTER GetChildClasses (pcObjectTypeGuids AS CHARACTER):

        DEFINE VARIABLE oParameter AS GetChildClassesParameter NO-UNDO .
        DEFINE VARIABLE hDataset   AS HANDLE                   NO-UNDO .

        IF CAN-FIND (ttChildClasses WHERE ttChildClasses.ParentClassNames = pcObjectTypeGuids) THEN DO:

            FIND ttChildClasses WHERE ttChildClasses.ParentClassNames = pcObjectTypeGuids NO-ERROR .
            RETURN ttChildClasses.ChildClassNames .
        END.

        oParameter = NEW GetChildClassesParameter(pcObjectTypeGuids) .

        IF CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U,
                                           "GetChildClasses":U,
                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                           oParameter) .
        ELSE
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity":U,
                                                           "GetChildClasses":U,
                                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                           oParameter) .

        CREATE ttChildClasses .
        ASSIGN ttChildClasses.ParentClassNames = pcObjectTypeGuids
               ttChildClasses.ChildClassNames  = oParameter:ChildObjectTypeGuids .
        RELEASE ttChildClasses .

        RETURN oParameter:ChildObjectTypeGuids .

    END METHOD .

    /**
     * Purpose: Returns the name fo the field object used to render
     *          a given Business Entity Field
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param pcEntityTable The name of the Business Entity Temp-Table
     * @param pcFieldName The name of the Business Entity Field
     * @param pcEnvironment The type of the rendering environment (GUI or WEB)
     * @return The name of the field object used to render the business entity field
     */
    METHOD PUBLIC CHARACTER GetEntityFieldMapping (pcEntityName AS CHARACTER,
                                                   pcEntityTable AS CHARACTER,
                                                   pcFieldName AS CHARACTER,
                                                   pcEnvironment AS CHARACTER):

        DEFINE VARIABLE oField       AS EntityFieldMappingDatasetModel NO-UNDO .
        DEFINE VARIABLE oStopwatch   AS Stopwatch                      NO-UNDO .
        DEFINE VARIABLE cQueryString AS CHARACTER                      NO-UNDO .

        IF lLogging THEN
            ASSIGN oStopwatch = NEW Stopwatch() .

        ASSIGN cQueryString = SUBSTITUTE ("for each eSmartEntityFieldMapping where eSmartEntityFieldMapping.FieldName = &1":U +
                                                                                            " and &2 matches eSmartEntityFieldMapping.EntityTable":U +
                                                                                            " and &3 matches eSmartEntityFieldMapping.EntityName":U +
                                                                                            " and eSmartEntityFieldMapping.Environments matches '*&4*'":U,
                                         QUOTER (pcFieldName),
                                         QUOTER (pcEntityTable),
                                         QUOTER (pcEntityName),
                                         pcEnvironment)

               oField = NEW EntityFieldMappingDatasetModel () .

        IF NOT oField:SmartEntityFieldMapping:CanFind (cQueryString) THEN
            RETURN ? .

        oField:SmartEntityFieldMapping:Fill (cQueryString) .

        IF lLogging THEN
            LogManager:WriteFormattedMessage ("[GetEntityFieldMapping] &1 / &2 / &3 / &4 : &5 (&6 msec)":U,
                                              pcEntityName,
                                              pcEntityTable,
                                              pcFieldName,
                                              pcEnvironment,
                                              oField:SmartEntityFieldMapping:FieldObjectNameWhenAvailable(?),
                                              STRING (oStopwatch:Elapsed),
                                              NEW MessageSubSystem ("SmartRepositoryService":U)) .

        IF oField:SmartEntityFieldMapping:Available THEN
            RETURN oField:SmartEntityFieldMapping:FieldObjectName .
        ELSE
            RETURN ? .

    END METHOD.

    /**
     * Purpose: Renders a repository field as html
     * Notes:   Fetches the Object Master Attributes from the respository
     * @param pcObjectName The name of the repository field
     * @param pcScope The scope to bind the field to (for additional properties)
     * @param pcFieldModel The model to bind the field to
     * @param pcFieldId  The Id of the field (JQuery)
     * @param plMandatory Logical value indicating if the field is mandatory
     * @param plReadOnly Logical value indicating if the field is mandatory
     * @param phDataset The handle of the Business Entity Dataset
     * @param pcTables The list of tables (entity table + entity view)
     * @param poBindingStyle The style of data bindings (AngularJS or Angular2)
     * @return The longchar representation (hmtl code) of the field
     */
    METHOD PUBLIC LONGCHAR GetHtmlField (pcObjectName AS CHARACTER,
                                         pcScope AS CHARACTER,
                                         pcFieldModel AS CHARACTER,
                                         pcFieldId AS CHARACTER,
                                         plMandatory AS LOGICAL,
                                         plReadOnly AS LOGICAL,
                                         phDataset AS HANDLE,
                                         pcTables AS CHARACTER,
                                         poBindingStyle AS ViewerBindingStyleEnum) .

        DEFINE VARIABLE oObjectMaster     AS ObjectMasterDatasetModel     NO-UNDO .
        DEFINE VARIABLE oCacheFactory     AS IRepositoryCacheTableFactory NO-UNDO .
        DEFINE VARIABLE hDataset          AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE hBuffer           AS HANDLE                       NO-UNDO .
        DEFINE VARIABLE oStopwatch        AS Stopwatch                    NO-UNDO .

        oCacheFactory = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.Class.IRepositoryCacheTableFactory
                                                      "NEW RepositoryCacheTableFactory ()"} .

        oObjectMaster = NEW ObjectMasterDatasetModel () .
        oObjectMaster:SmartObjectMaster:Filter:ObjectName:EQ (pcObjectName):Run() .

        IF lLogging THEN
            oStopwatch =  NEW Stopwatch() .

        hDataset = oCacheFactory:GetRepositoryCacheTables
                        (GetAttributesForEnum:RunTime,
                         NEW CharacterList (oObjectMaster:SmartObjectMaster:ObjectTypeName)) .

        IF lLogging THEN
            LogManager:WriteFormattedMessage ("[GetRepositoryCacheTables] &1 (&2 msec)":U,
                                              pcObjectName,
                                              STRING (oStopwatch:Elapsed),
                                              NEW MessageSubSystem ("SmartRepositoryService":U)) .

        hBuffer = hDataset:GET-BUFFER-HANDLE (oObjectMaster:SmartObjectMaster:ObjectTypeName) .

        hBuffer:BUFFER-CREATE () .

        ASSIGN
            hBuffer::_ObjectMasterGuid = oObjectMaster:SmartObjectMaster:ObjectMasterGuid .

        IF lLogging THEN
            oStopwatch =  NEW Stopwatch() .

        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
                                       "FetchObjectMasterAttributes":U,
                                       INPUT-OUTPUT DATASET-HANDLE hDataset BY-REFERENCE, ?) .

        IF lLogging THEN
            LogManager:WriteFormattedMessage ("[FetchObjectMasterAttributes] &1 (&2 msec)":U,
                                              pcObjectName,
                                              STRING (oStopwatch:Elapsed),
                                              NEW MessageSubSystem ("SmartRepositoryService":U)) .

        hBuffer:FIND-FIRST () .

        RETURN THIS-OBJECT:GetHtmlField (pcObjectName,
                                         pcScope,
                                         pcFieldModel,
                                         pcFieldId,
                                         plMandatory,
                                         plReadOnly,
                                         phDataset,
                                         pcTables,
                                         poBindingStyle,
                                         hBuffer) .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Renders a repository field as html
     * Notes:   Uses an externally provided attribute buffer (which can be an object master or an object instance)
     * @param pcObjectName The name of the repository field
     * @param pcScope The scope to bind the field to (for additional properties)
     * @param pcFieldModel The model to bind the field to
     * @param pcFieldId  The Id of the field (JQuery)
     * @param plMandatory Logical value indicating if the field is mandatory
     * @param plReadOnly Logical value indicating if the field is mandatory
     * @param phDataset The handle of the Business Entity Dataset
     * @param pcTables The list of tables (entity table + entity view)
     * @param poBindingStyle The style of data bindings (AngularJS or Angular2)
     * @param phAttributesBuffer The buffer with the attributes of the object
     * @return The longchar representation (hmtl code) of the field
     */
    METHOD PUBLIC LONGCHAR GetHtmlField (pcObjectName AS CHARACTER,
                                         pcScope AS CHARACTER,
                                         pcFieldModel AS CHARACTER,
                                         pcFieldId AS CHARACTER,
                                         plMandatory AS LOGICAL,
                                         plReadOnly AS LOGICAL,
                                         phDataset AS HANDLE,
                                         pcTables AS CHARACTER,
                                         poBindingStyle AS ViewerBindingStyleEnum,
                                         phAttributesBuffer AS HANDLE) .

        DEFINE VARIABLE oRenderingService AS IWebRenderingService         NO-UNDO .

        DEFINE VARIABLE oStopwatch        AS Stopwatch                    NO-UNDO .

        IF lLogging THEN
            ASSIGN oStopwatch  = NEW Stopwatch() .

        oRenderingService = CAST (FrameworkSettings:ServiceContainer:GetService(Progress.Lang.Class:GetClass (phAttributesBuffer::WebRenderingService)),
                                  IWebRenderingService) .

        IF NOT VALID-OBJECT (oRenderingService) THEN DO:
            oRenderingService = DYNAMIC-NEW (phAttributesBuffer::WebRenderingService) () .
            FrameworkSettings:ServiceContainer:AddService (Progress.Lang.Class:GetClass (phAttributesBuffer::WebRenderingService),
                                                           oRenderingService) .
        END.

        RETURN oRenderingService:RenderHtml (pcObjectName,
                                             pcScope,
                                             pcFieldModel,
                                             pcFieldId,
                                             plMandatory,
                                             plReadOnly,
                                             phAttributesBuffer,
                                             phDataset,
                                             pcTables,
                                             poBindingStyle) .

        CATCH err AS Progress.Lang.Error:
            LogManager:WriteSeparator() .
            LogManager:WriteFormattedMessage("*** Error rendering: &1":U, pcObjectName) .
            LogManager:WriteBuffer(phAttributesBuffer, TRUE) .
            LogManager:WriteSeparator() .

            UNDO, THROW err .
        END CATCH.

        FINALLY:
            IF lLogging THEN
                LogManager:WriteFormattedMessage ("[GetHtmlField] &1 / &2 (&3 msec)":U,
                                                  pcObjectName,
                                                  oRenderingService:GetClass():TypeName,
                                                  STRING (oStopwatch:Elapsed),
                                                  NEW MessageSubSystem ("SmartRepositoryService":U)) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Returns the Object Master Guid based on an Object Master Name
     *          or an object master guid
     * Notes:
     * @param pcObjectMaster The Object Master Name or Object Master Guid
     * @return The Object Master Guid
     */
    METHOD PUBLIC CHARACTER GetObjectMasterGuid (pcObjectMaster AS CHARACTER):

        DEFINE VARIABLE oObjectMaster AS ObjectMasterDatasetModel NO-UNDO .

        oObjectMaster = NEW ObjectMasterDatasetModel (pcObjectMaster) .

        IF oObjectMaster:SmartObjectMaster:Available THEN
            RETURN pcObjectMaster .

        oObjectMaster:SmartObjectMaster:Filter:Reset ().
        oObjectMaster:SmartObjectMaster:Filter:ObjectName:EQ (pcObjectMaster):Run () .

        IF oObjectMaster:SmartObjectMaster:Available THEN
            RETURN oObjectMaster:SmartObjectMaster:ObjectMasterGuid .

        UNDO, THROW NEW InvalidParameterValueException ("pcObjectMaster":U, pcObjectMaster, THIS-OBJECT:GetClass():TypeName) .

    END METHOD .

    /**
     * Purpose: Returns the Object Type Guid based on an Object Type Name
     *          or an object type guid
     * Notes:
     * @param pcObjectType The Object Type Name or Object Type Guid
     * @return The Object Type Guid
     */
    METHOD PUBLIC CHARACTER GetObjectTypeGuid (pcObjectType AS CHARACTER):

        DEFINE VARIABLE oObjectType AS ObjectTypeDatasetModel NO-UNDO .

        oObjectType = NEW ObjectTypeDatasetModel (pcObjectType) .

        IF oObjectType:SmartObjectType:Available THEN
            RETURN pcObjectType .

        oObjectType:SmartObjectType:Filter:Reset ().
        oObjectType:SmartObjectType:Filter:ObjectTypeName:EQ (pcObjectType):Run () .

        IF oObjectType:SmartObjectType:Available THEN
            RETURN oObjectType:SmartObjectType:ObjectTypeGuid .

        UNDO, THROW NEW InvalidParameterValueException ("pcObjectType":U, pcObjectType, THIS-OBJECT:GetClass():TypeName) .

    END METHOD .

    /**
     * Purpose: Returns the Object Type Guid of the given object master
     * Notes:   Supports object master guid and object master name
     * @param pcObjectMaster The obejct master guid or name
     * @return The object type guid of the given object master
     */
    METHOD PUBLIC CHARACTER GetObjectTypeGuidOfMaster (pcObjectMaster AS CHARACTER):

        DEFINE VARIABLE oObjectMaster AS ObjectMasterDatasetModel NO-UNDO .

        oObjectMaster = NEW ObjectMasterDatasetModel (pcObjectMaster).

        IF oObjectMaster:SmartObjectMaster:Available THEN
            RETURN oObjectMaster:SmartObjectMaster:ObjectTypeGuid .

        oObjectMaster:SmartObjectMaster:Filter:Reset () .
        oObjectMaster:SmartObjectMaster:Filter:ObjectName:EQ (pcObjectMaster):Run () .

        IF oObjectMaster:SmartObjectMaster:Available THEN
            RETURN oObjectMaster:SmartObjectMaster:ObjectTypeGuid .

        UNDO, THROW NEW InvalidParameterValueException ("pcObjectMaster":U, pcObjectMaster, THIS-OBJECT:GetClass():TypeName) .

    END METHOD .

    /**
     * Purpose: Returns the Data Folder of the given Product Module
     * Notes:   Throws an exception, when the module does not have a data folder
     * @param pcModuleGuid The guid of the Product Module
     * @return The data folder of the given product module
     */
    METHOD PUBLIC CHARACTER GetProductModuleDataFolder (pcModuleGuid AS CHARACTER).

        DEFINE VARIABLE oModule          AS ModuleDatasetModel NO-UNDO .
        DEFINE VARIABLE oMessageProvider AS IMessageProvider   NO-UNDO .

        oModule = NEW ModuleDatasetModel (pcModuleGuid) .

        BufferAssert:IsAvailable (oModule:SmartModule) .

        IF oModule:SmartModule:ModuleDataFolder > "":U THEN
            RETURN oModule:SmartModule:ModuleDataFolder .

        ASSIGN oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider
                                                                "NEW MessageProvider ()"} .

        UNDO, THROW NEW MissingModuleDataFolderException (oMessageProvider:GetMessageText("SFR":U, 404, oModule:SmartModule:ModuleName),
                                                          404,
                                                          oModule:SmartModule:ModuleName) .

    END METHOD .

    /**
     * Purpose: Returns the Product Module Guid based on an Product Module Name
     *          or an product module guid
     * Notes:
     * @param pcProductModule The Product Module Name or Product Module Guid
     * @return The Product Module Guid
     */
    METHOD PUBLIC CHARACTER GetProductModuleGuid (pcProductModule AS CHARACTER):

        DEFINE VARIABLE oModule AS ModuleDatasetModel NO-UNDO .

        oModule = NEW ModuleDatasetModel (pcProductModule) .

        IF oModule:SmartModule:Available THEN
            RETURN pcProductModule .

        oModule:SmartModule:Filter:Reset ().
        oModule:SmartModule:Filter:ModuleName:EQ (pcProductModule):Run () .

        IF oModule:SmartModule:Available THEN
            RETURN oModule:SmartModule:ModuleGuid .

        UNDO, THROW NEW InvalidParameterValueException ("pcProductModule":U, pcProductModule, THIS-OBJECT:GetClass():TypeName) .

    END METHOD .

    /**
     * Purpose: Returns if the object is of the given class
     * Notes:
     * @param pcObjectName The name of the object to verify
     * @param pcClassName The name of the class to test if the object is a child of
     * @return Logical value indicating if the object is child of the given class
     */
    METHOD PUBLIC LOGICAL ObjectIsA (pcObjectName AS CHARACTER,
                                     pcClassName AS CHARACTER):

        DEFINE VARIABLE oParameter AS ObjectIsAParameter NO-UNDO .
        DEFINE VARIABLE hDataset   AS HANDLE             NO-UNDO .

        IF CAN-FIND (ttObjectIsA WHERE ttObjectIsA.ObjectName = pcObjectName
                                   AND ttObjectIsA.ClassName  = pcClassName) THEN DO:

            FIND ttObjectIsA WHERE ttObjectIsA.ObjectName = pcObjectName
                               AND ttObjectIsA.ClassName  = pcClassName NO-ERROR .

            RETURN ttObjectIsA.IsA .
        END.

        ASSIGN oParameter = NEW ObjectIsAParameter(pcObjectName, pcClassName) .

        IF CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U,
                                           "ObjectIsA":U,
                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                           oParameter) .
        ELSE
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U,
                                                           "ObjectIsA":U,
                                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                           oParameter) .

        CREATE ttObjectIsA .
        ASSIGN ttObjectIsA.ObjectName = pcObjectName
               ttObjectIsA.ClassName  = pcClassName
               ttObjectIsA.IsA        = oParameter:IsA .

        RELEASE ttObjectIsA .

        RETURN oParameter:IsA .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

END CLASS.
