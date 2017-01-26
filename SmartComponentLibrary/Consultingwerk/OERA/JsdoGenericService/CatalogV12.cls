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
    File        : CatalogV12
    Purpose     : Generic JSON/JSDO Catalog generation, V1.2 (OpenEdge 11.5)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 19 11:51:31 CEST 2015
    Notes       : Supports extensions for Kendo UI dialect of the JSDO
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                     FROM PROPATH .
USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.Framework.Collections.*         FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.*       FROM PROPATH .
USING Consultingwerk.OERA.OpenAccess.*               FROM PROPATH .
USING Progress.Json.ObjectModel.*                    FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.OERA.JsdoGenericService.CatalogV12
    IMPLEMENTS ICatalog:

    DEFINE VARIABLE cCountMethodType AS CHARACTER NO-UNDO .

    /**
     * Purpose: Returns the Version of the JSDO Catalog format
     * Notes:
     */
    DEFINE PUBLIC PROPERTY CatalogVersion AS CHARACTER NO-UNDO INITIAL "1.2":U
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns the name of the Business Entity currently processed
     * Notes:
     */
    DEFINE PROTECTED PROPERTY EntityName AS CHARACTER NO-UNDO INITIAL ?
    GET.
    SET.

    /**
     * Purpose: Returns the Catalog for a given Business Entity
     * Notes:   Defaults to the count method type of the .restapplicationsettings file
     * @param pcEntityName The name of the Business Entity
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @return The JSON Object describing the Catalog for the Business Entity
     */
    METHOD PUBLIC JsonObject GetCatalogForBusinessEntity (pcEntityName AS CHARACTER,
                                                          poAddress AS RestServiceAddress) .

        RETURN THIS-OBJECT:GetCatalogForBusinessEntity (pcEntityName, poAddress, ?) .

    END METHOD .

    /**
     * Purpose: Returns the Catalog for a given Business Entity
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @param pcCountMethodType The type (count/invoke) for the count method
     * @return The JSON Object describing the Catalog for the Business Entity
     */
    METHOD PUBLIC JsonObject GetCatalogForBusinessEntity (pcEntityName AS CHARACTER,
                                                          poAddress AS RestServiceAddress,
                                                          pcCountMethodType AS CHARACTER) .

        DEFINE VARIABLE hDataset      AS HANDLE       NO-UNDO .
        DEFINE VARIABLE hDataset2     AS HANDLE       NO-UNDO .
        DEFINE VARIABLE oJsonObject   AS JsonObject   NO-UNDO .
        DEFINE VARIABLE oServices     AS JsonArray    NO-UNDO .
        DEFINE VARIABLE oSchemaHelper AS SchemaHelper NO-UNDO .

        ASSIGN cCountMethodType = pcCountMethodType .

        oSchemaHelper = NEW SchemaHelper () .

        ServiceInterface:FetchDataset (pcEntityName, OUTPUT DATASET-HANDLE hDataset) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hDataset """hDataset"":U"} .

        hDataset2 = oSchemaHelper:AddFieldsToDataset (hDataset, "id,seq":U) .

        oJsonObject = NEW JsonObject () .
        oServices   = NEW JsonArray  () .

        oJsonObject:Add ("version":U, THIS-OBJECT:CatalogVersion) .
        oJsonObject:Add("lastModified":U, ISO-DATE(NOW)).

        oJsonObject:Add ("services":U, oServices) .

        oServices:Add (THIS-OBJECT:GetService (pcEntityName,
                                               poAddress,
                                               hDataset2)) .

        RETURN oJsonObject .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
            IF VALID-HANDLE (hDataset2) THEN
                DELETE OBJECT hDataset2 .

            ASSIGN cCountMethodType = ? .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Returns the Catalog for a given Business Entity
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @param pcJsonCatalog OUTPUT The JSON Catalog for the Business Entity
     */
    METHOD PUBLIC VOID GetCatalogForBusinessEntity (pcEntityName AS CHARACTER,
                                                    poAddress AS RestServiceAddress,
                                                    OUTPUT pcJsonCatalog AS LONGCHAR):

        DEFINE VARIABLE oJsonObject   AS JsonObject   NO-UNDO .

        oJsonObject = THIS-OBJECT:GetCatalogForBusinessEntity(pcEntityName, poAddress) .

        oJsonObject:Write (pcJsonCatalog, TRUE) .

    END METHOD .

    /**
     * Purpose: Returns the Catalog for a given Business Task
     * Notes:
     * @param poBusinessTask The name of the Business Task
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @return The JSON Object describing the Catalog for the Business Task
     */
    METHOD PUBLIC JsonObject GetCatalogForBusinessTask (poBusinessTask AS IBusinessTaskCatalogData,
                                                        poAddress AS RestServiceAddress):

        DEFINE VARIABLE hDataset      AS HANDLE        NO-UNDO .
        DEFINE VARIABLE hDataset2     AS HANDLE        NO-UNDO .
        DEFINE VARIABLE oJsonObject   AS JsonObject    NO-UNDO .
        DEFINE VARIABLE oServices     AS JsonArray     NO-UNDO .
        DEFINE VARIABLE oSchemaHelper AS SchemaHelper  NO-UNDO .

        DEFINE VARIABLE oDatasets     AS HandleList    NO-UNDO .
        DEFINE VARIABLE oDatasetNames AS CharacterList NO-UNDO .

        DEFINE VARIABLE i             AS INTEGER       NO-UNDO .

        ObjectAssert:IsValid (poBusinessTask, "poBusinessTask":U) .

        ASSIGN oSchemaHelper = NEW SchemaHelper ()

               oDatasets     = NEW HandleList ()
               oDatasetNames = NEW CharacterList () .

        {Consultingwerk/foreachABL.i IBusinessTaskMethodData oMethod in poBusinessTask:Methods}

            DO i = 1 TO EXTENT (oMethod:DatasetHandles):
                ASSIGN hDataset = oMethod:DatasetHandles[i] .

                IF NOT oDatasetNames:ContainsValue(hDataset:NAME) THEN DO:

                    hDataset2 = oSchemaHelper:AddFieldsToDataset (hDataset, "id,seq":U) .
                    oDatasetNames:Add(hDataset:NAME) .
                    oDatasets:Add (hDataset2) .
                END.
            END.
        END.

        oJsonObject = NEW JsonObject () .
        oServices   = NEW JsonArray  () .

        oJsonObject:Add ("version":U, THIS-OBJECT:CatalogVersion) .
        oJsonObject:Add("lastModified":U, ISO-DATE(NOW)).

        oJsonObject:Add ("services":U, oServices) .

        oServices:Add (THIS-OBJECT:GetService (poBusinessTask,
                                               poAddress,
                                               oDatasets)) .

        RETURN oJsonObject .

        FINALLY:
            {Consultingwerk/foreachPrimitiveList.i Handle hDatasetHandle IN oDatasets}
                IF VALID-HANDLE (hDatasetHandle) THEN
                    DELETE OBJECT hDatasetHandle .
            END .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Returns the JSON Schema description for the ProDataset
     * Notes:   Does not include the schema envelope
     * @return The JsonObject representing the schema
     * @param phDataset The ProDataset to return the schema for
     */
    METHOD PUBLIC JsonObject GetDatasetSchema (phDataset AS HANDLE):

        DEFINE VARIABLE oDataset                 AS JsonObject              NO-UNDO.
        DEFINE VARIABLE oProperties2             AS JsonObject              NO-UNDO.

        DEFINE VARIABLE oTempTableSchemaExporter AS TempTableSchemaExporter NO-UNDO .
        DEFINE VARIABLE hBuffer                  AS HANDLE                  NO-UNDO .
        DEFINE VARIABLE cType                    AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE i                        AS INTEGER                 NO-UNDO .

        oDataset = NEW JsonObject () .
        oProperties2 = NEW JsonObject () .

        oDataset:Add ("type":U, "object":U) .
        oDataset:Add ("additionalProperties":U, FALSE) .
        oDataset:Add ("properties":U, oProperties2) .

        oTempTableSchemaExporter = NEW TempTableSchemaExporter () .
        oTempTableSchemaExporter:IncludeErrorString = TRUE .

        DO i = 1 TO phDataset:NUM-BUFFERS:
            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (i) .

            IF VALID-HANDLE (hBuffer:PARENT-RELATION) AND hBuffer:PARENT-RELATION:NESTED THEN
                ASSIGN cType = "object":U .
            ELSE
                ASSIGN cType = "array":U .

            oTempTableSchemaExporter:TempTableSchemaToJsonObject (hBuffer,
                                                                  cType,
                                                                  oProperties2) .
        END.

        RETURN oDataset .

    END METHOD .

    /**
     * Purpose: Returns an Operation with the given parameters
     * Notes:
     * @param pcPath The path of the operation
     * @param plUseBeforeImage Logical value indicating the use of before image
     * @param pcType The type of the operation
     * @param pcVerb The http verb of the operation
     * @param pcParameterNames The names of the operation parameters
     * @param pcParameterTypes The types of the operation parameters
     * @return The JsonObject representing an operation
     */
    METHOD PUBLIC JsonObject GetOperation (pcPath AS CHARACTER,
                                           plUseBeforeImage AS LOGICAL,
                                           pcType AS CHARACTER,
                                           pcVerb AS CHARACTER,
                                           pcParameterNames AS CHARACTER EXTENT,
                                           pcParameterTypes AS CHARACTER EXTENT):

        RETURN THIS-OBJECT:GetOperation (pcPath,
                                         plUseBeforeImage,
                                         pcType,
                                         pcVerb,
                                         "":U,
                                         pcParameterNames,
                                         pcParameterTypes) .
    END METHOD .

    /**
     * Purpose: Returns an Operation with the given parameters
     * Notes:
     * @param pcPath The path of the operation
     * @param plUseBeforeImage Logical value indicating the use of before image
     * @param pcType The type of the operation
     * @param pcVerb The http verb of the operation
     * @param pcName The method name
     * @param pcParameterNames The names of the operation parameters
     * @param pcParameterTypes The types of the operation parameters
     * @return The JsonObject representing an operation
     */
    METHOD PUBLIC JsonObject GetOperation (pcPath AS CHARACTER,
                                           plUseBeforeImage AS LOGICAL,
                                           pcType AS CHARACTER,
                                           pcVerb AS CHARACTER,
                                           pcName AS CHARACTER,
                                           pcParameterNames AS CHARACTER EXTENT,
                                           pcParameterTypes AS CHARACTER EXTENT):

        DEFINE VARIABLE oOperation  AS JsonObject NO-UNDO.
        DEFINE VARIABLE oParameters AS JsonArray  NO-UNDO.
        DEFINE VARIABLE oParameter  AS JsonObject NO-UNDO.

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        oOperation = NEW JsonObject () .
        oParameters = NEW JsonArray () .

        IF pcName > "":U THEN
            oOperation:Add ("name":U, pcName) .

        oOperation:Add ("path":U, pcPath) .
        oOperation:Add ("useBeforeImage":U, plUseBeforeImage) .
        oOperation:Add ("type":U, pcType) .
        oOperation:Add ("verb":U, pcVerb) .
        oOperation:Add ("params":U, oParameters) .

        DO i = 1 TO EXTENT (pcParameterNames):

            oParameter = NEW JsonObject () .
            oParameter:Add ("name":U, pcParameterNames[i]) .
            oParameter:Add ("type":U, pcParameterTypes[i]) .

            oParameters:Add (oParameter) .

        END.

        RETURN oOperation.

    END METHOD .

    /**
     * Purpose: Returns an Operation with the given parameters
     * Notes:
     * @param pcPath The path of the operation
     * @param plUseBeforeImage Logical value indicating the use of before image
     * @param pcType The type of the operation
     * @param pcVerb The http verb of the operation
     * @return The JsonObject representing an operation
     */
    METHOD PUBLIC JsonObject GetOperation (pcPath AS CHARACTER,
                                           plUseBeforeImage AS LOGICAL,
                                           pcType AS CHARACTER,
                                           pcVerb AS CHARACTER):

        DEFINE VARIABLE cParameterName  AS CHARACTER EXTENT NO-UNDO .
        DEFINE VARIABLE cParameterTypes AS CHARACTER EXTENT NO-UNDO .

        RETURN THIS-OBJECT:GetOperation (pcPath,
                                         plUseBeforeImage,
                                         pcType,
                                         pcVerb,
                                         cParameterName,
                                         cParameterTypes) .

    END METHOD .

    /**
     * Purpose: Returns an Operation with the given parameters
     * Notes:
     * @param pcPath The path of the operation
     * @param plUseBeforeImage Logical value indicating the use of before image
     * @param pcType The type of the operation
     * @param pcVerb The http verb of the operation
     * @param pcName The name of the operation
     * @return The JsonObject representing an operation
     */
    METHOD PUBLIC JsonObject GetOperation (pcPath AS CHARACTER,
                                           plUseBeforeImage AS LOGICAL,
                                           pcType AS CHARACTER,
                                           pcVerb AS CHARACTER,
                                           pcName AS CHARACTER):

        DEFINE VARIABLE cParameterName  AS CHARACTER EXTENT NO-UNDO .
        DEFINE VARIABLE cParameterTypes AS CHARACTER EXTENT NO-UNDO .

        RETURN THIS-OBJECT:GetOperation (pcPath,
                                         plUseBeforeImage,
                                         pcType,
                                         pcVerb,
                                         pcName,
                                         cParameterName,
                                         cParameterTypes) .

    END METHOD .

    /**
     * Purpose: Returns the Array of REST Operations for the given Dataset Name
     * Notes:
     * @param pcEntityName The name of the business entity
     * @param pcDatasetName The name of the ProDataset parameter
     * @return The JsonArray representing the operations
     */
    METHOD PUBLIC JsonArray GetOperations (pcEntityName AS CHARACTER,
                                           pcDatasetName AS CHARACTER):

        DEFINE VARIABLE oOperations AS JsonArray                            NO-UNDO .
        DEFINE VARIABLE oRead       AS JsonObject                           NO-UNDO .

        DEFINE VARIABLE hDataset    AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oParameter  AS GetBusinessEntityDescriptorParameter NO-UNDO .

        DEFINE VARIABLE cCount      AS CHARACTER                            NO-UNDO INITIAL "invoke":U .
        DEFINE VARIABLE oConfig     AS IConfigurationProvider               NO-UNDO .

        oConfig = {Consultingwerk/get-service.i IConfigurationProvider} .

        IF cCountMethodType <> ? THEN
            ASSIGN cCount = cCountMethodType .
        ELSE IF VALID-OBJECT (oConfig) THEN
            ASSIGN cCount = oConfig:GetValue("CountFunctionJsonCatalogType":U, "invoke":U) .

        oOperations = NEW JsonArray () .

        oOperations:Add (THIS-OBJECT:GetOperation ("/count?filter=~{filter}":U,
                                                   FALSE,
                                                   cCount,
                                                   "put":U,
                                                   "count":U)) .

        oOperations:Add (THIS-OBJECT:GetOperation ("":U,
                                                   TRUE,
                                                   "update":U,
                                                   "put":U,
                                                   ArrayHelper:Array(pcDatasetName),
                                                   ArrayHelper:Array("REQUEST_BODY":U))) .

        oOperations:Add (THIS-OBJECT:GetOperation ("":U,
                                                   TRUE,
                                                   "create":U,
                                                   "post":U,
                                                   ArrayHelper:Array(pcDatasetName),
                                                   ArrayHelper:Array("REQUEST_BODY":U))) .


        oOperations:Add (THIS-OBJECT:GetOperation ("":U,
                                                   TRUE,
                                                   "delete":U,
                                                   "delete":U,
                                                   ArrayHelper:Array(pcDatasetName),
                                                   ArrayHelper:Array("REQUEST_BODY":U))) .

        oOperations:Add (THIS-OBJECT:GetOperation ("/SubmitData":U,
                                                   TRUE,
                                                   "submit":U,
                                                   "post":U,
                                                   ArrayHelper:Array(pcDatasetName),
                                                   ArrayHelper:Array("REQUEST_BODY":U))) .

        oRead = THIS-OBJECT:GetOperation ("?filter=~{filter}":U,
                                                   TRUE,
                                                   "read":U,
                                                   "get":U) .
        oRead:Add ("mappingType":U, "JFP":U) .
        oRead:Add ("capabilities":U, "ablFilter,top,skip,id,orderBy":U) .
        oOperations:Add (oRead) .

        oParameter = NEW GetBusinessEntityDescriptorParameter () .

        ServiceInterface:InvokeMethod (pcEntityName,
                                       "GetBusinessEntityDescriptor":U,
                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                       oParameter) .

        IF VALID-HANDLE (hDataset) THEN
            DELETE OBJECT hDataset .

        IF VALID-OBJECT (oParameter) AND VALID-OBJECT (oParameter:Descriptor) THEN DO:

            {Consultingwerk/foreachABL.i IInvokableMethodDescriptor oMethod in oParameter:Descriptor:InvokableMethods}
                IF oMethod:DatasetInput THEN
                    oOperations:Add (THIS-OBJECT:GetOperation (SUBSTITUTE ("/&1":U, oMethod:Name),
                                                               FALSE,
                                                               "invoke":U,
                                                               "put":U,
                                                               oMethod:Name,
                                                               ArrayHelper:Array("plcDataset":U, "plcParameter":U),
                                                               ArrayHelper:Array("REQUEST_BODY":U,"REQUEST_BODY":U))) .
                ELSE
                    oOperations:Add (THIS-OBJECT:GetOperation (SUBSTITUTE ("/&1":U, oMethod:Name),
                                                               FALSE,
                                                               "invoke":U,
                                                               "put":U,
                                                               oMethod:Name,
                                                               ArrayHelper:Array("plcParameter":U),
                                                               ArrayHelper:Array("REQUEST_BODY":U))) .
            END.
        END.

        RETURN oOperations .

    END METHOD .

    /**
     * Purpose: Returns the Array of REST Operations for the given Dataset Name
     * Notes:
     * @param poBusinessTask The IBusinessTaskCatalogData instance that describes the operations
     * @return The JsonArray representing the operations
     */
    METHOD PUBLIC JsonArray GetOperations (poBusinessTask AS IBusinessTaskCatalogData):

        DEFINE VARIABLE oOperations     AS JsonArray     NO-UNDO .
        DEFINE VARIABLE oParameterNames AS CharacterList NO-UNDO .
        DEFINE VARIABLE oParameterTypes AS CharacterList NO-UNDO .
        DEFINE VARIABLE i               AS INTEGER       NO-UNDO .

        ASSIGN oOperations = NEW JsonArray ()
               oParameterNames = NEW CharacterList ()
               oParameterTypes = NEW CharacterList ()
               .

        {Consultingwerk/foreachABL.i IBusinessTaskMethodData oMethod in poBusinessTask:Methods}

            oParameterNames:Clear() .
            oParameterNames:Clear().

            DO i = 1 TO EXTENT (oMethod:DatasetHandles):

                IF oMethod:DatasetParameterMode[i] = DatasetParameterModeEnum:Input OR
                   oMethod:DatasetParameterMode[i] = DatasetParameterModeEnum:InputOutput THEN DO:

                    oParameterNames:Add(oMethod:DatasetHandles[i]:NAME) .
                    oParameterTypes:Add("REQUEST_BODY":U) .
                END.
            END.

            oParameterNames:Add("plcParameter":U) .
            oParameterTypes:Add("REQUEST_BODY":U) .

            oOperations:Add (THIS-OBJECT:GetOperation (SUBSTITUTE ("/&1":U, oMethod:MethodName),
                                                       FALSE,
                                                       "invoke":U,
                                                       "put":U,
                                                       oMethod:MethodName,
                                                       oParameterNames:ToArray(),
                                                       oParameterTypes:ToArray())) .
        END.

        RETURN oOperations .

    END METHOD .

    /**
     * Purpose: Returns the JSON Schema description of the Relations for the ProDataset
     * Notes:
     * @param phDataset The ProDataset to return the schema for
     * @return The JsonObject representing the relations
     */
    METHOD PUBLIC JsonArray GetRelations (phDataset AS HANDLE):

        DEFINE VARIABLE oRelations      AS JsonArray  NO-UNDO .
        DEFINE VARIABLE oRelation       AS JsonObject NO-UNDO .
        DEFINE VARIABLE hRelation       AS HANDLE     NO-UNDO .
        DEFINE VARIABLE oRelationFields AS JsonArray  NO-UNDO .
        DEFINE VARIABLE oRelationField  AS JsonObject NO-UNDO .
        DEFINE VARIABLE i               AS INTEGER    NO-UNDO .
        DEFINE VARIABLE j               AS INTEGER    NO-UNDO .
        DEFINE VARIABLE cParentField    AS CHARACTER  NO-UNDO .
        DEFINE VARIABLE cChildField     AS CHARACTER  NO-UNDO .

        oRelations = NEW JsonArray () .

        DO i = 1 TO phDataset:NUM-RELATIONS:

            ASSIGN hRelation = phDataset:GET-RELATION (i) .

            oRelation = NEW JsonObject () .
            oRelationFields = NEW JsonArray () .

            oRelation:Add ("relationName":U, hRelation:NAME) .
            oRelation:Add ("parentName":U, hRelation:PARENT-BUFFER:NAME) .
            oRelation:Add ("childName":U, hRelation:CHILD-BUFFER:NAME) .
            oRelation:Add ("relationFields":U, oRelationFields) .

            DO j = 2 TO NUM-ENTRIES (hRelation:RELATION-FIELDS) BY 2:

                ASSIGN cParentField = BufferHelper:FieldSerializeName (hRelation:PARENT-BUFFER:BUFFER-FIELD (ENTRY (j - 1, hRelation:RELATION-FIELDS)))
                       cChildField  = BufferHelper:FieldSerializeName (hRelation:CHILD-BUFFER:BUFFER-FIELD  (ENTRY (j,      hRelation:RELATION-FIELDS))) . 

                oRelationField = NEW JsonObject () .
                oRelationField:Add ("parentFieldName":U, cParentField) .
                oRelationField:Add ("childFieldName":U, cChildField) .

                oRelationFields:Add (oRelationField) .

            END.

            oRelations:Add (oRelation) .

        END.

        RETURN oRelations .

    END METHOD .

    /**
     * Purpose: Returns a Resource instance
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param phDataset The dataset to return
     * @return The JsonObject representing the resources
     */
    METHOD PROTECTED JsonObject GetResource (pcEntityName AS CHARACTER,
                                             phDataset AS HANDLE):

        DEFINE VARIABLE oResource AS JsonObject NO-UNDO .
        DEFINE VARIABLE oRelations AS JsonArray  NO-UNDO .

        oResource = NEW JsonObject () .

        oResource:Add ("name":U, pcEntityName) .
        oResource:Add ("path":U, "/":U + pcEntityName) .
        oResource:Add ("autoSave":U, TRUE) .

        oResource:Add ("schema":U, THIS-OBJECT:GetSchema (phDataset)) .

        oRelations = THIS-OBJECT:GetRelations (phDataset) .

        IF oRelations:Length > 0 THEN
            oResource:Add ("relations":U, oRelations) .

        oResource:Add ("operations":U, THIS-OBJECT:GetOperations (pcEntityName,
                                                                  phDataset:NAME)) .

        RETURN oResource .

    END METHOD .

    /**
     * Purpose: Returns a Resource instance
     * Notes:
     * @param poBusinessTask The reference to the IBusinessTaskCatalogData
     * @param poDatasets The datasets to add to the catalog
     * @return The JsonObject representing the service
     */
    METHOD PROTECTED JsonObject GetResource (poBusinessTask AS IBusinessTaskCatalogData,
                                             poDatasets AS HandleList):

        DEFINE VARIABLE oResource AS JsonObject NO-UNDO .

        oResource = NEW JsonObject () .

        oResource:Add ("name":U, poBusinessTask:BusinessTaskName) .
        oResource:Add ("path":U, "/":U + poBusinessTask:BusinessTaskName) .
        oResource:Add ("autoSave":U, TRUE) .

        /* Business Task catalog is not supposed to contain ProDatasets and Relations
           It looks as if the JSDO does support only a single ProDataset per Resource
           Business Tasks may contain multiple Datasets. */
        /*oResource:Add ("schema":U, THIS-OBJECT:GetSchema (poDatasets))*/ .
        /*oResource:Add ("relations":U, THIS-OBJECT:GetRelations (poDatasets)) .*/

        oResource:Add ("operations":U, THIS-OBJECT:GetOperations (poBusinessTask)) .

        RETURN oResource .

    END METHOD .

    /**
     * Purpose: Returns the JSON Schema description for the ProDataset
     * Notes:
     * @param phDataset The ProDataset to return the schema for
     * @return The JsonObject representing the schema
     */
    METHOD PUBLIC JsonObject GetSchema (phDataset AS HANDLE):

        DEFINE VARIABLE oSchema                  AS JsonObject              NO-UNDO.
        DEFINE VARIABLE oProperties              AS JsonObject              NO-UNDO.
        DEFINE VARIABLE oDataset                 AS JsonObject              NO-UNDO.

        oSchema      = NEW JsonObject () .
        oProperties  = NEW JsonObject () .

        oSchema:Add ("type":U, "object":U) .
        oSchema:Add ("additionalProperties":U, FALSE) .
        oSchema:Add ("properties":U, oProperties) .

        oDataset = THIS-OBJECT:GetDatasetSchema (phDataset) .

        oProperties:Add (phDataset:NAME, oDataset) .

        RETURN oSchema .

    END METHOD .

    /**
     * Purpose: Returns a Service instsance
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @param phDataset The dataset to return
     * @return The JsonObject representing the service
     */
    METHOD PROTECTED JsonObject GetService (pcEntityName AS CHARACTER,
                                            poAddress AS RestServiceAddress,
                                            phDataset AS HANDLE):

        DEFINE VARIABLE oService    AS JsonObject NO-UNDO .
        DEFINE VARIABLE oResources  AS JsonArray  NO-UNDO .

        ASSIGN THIS-OBJECT:EntityName = pcEntityName .

        oService   = NEW JsonObject () .
        oResources = NEW JsonArray () .

        oService:Add ("name":U, poAddress:Name) .
        oService:Add ("address":U, poAddress:Address) .
        oService:Add ("useRequest":U, TRUE) .

        oService:Add ("resources":U, oResources) .

        oResources:Add (THIS-OBJECT:GetResource (pcEntityName,
                                                 phDataset)) .

        RETURN oService .

        FINALLY:
            ASSIGN THIS-OBJECT:EntityName = ? .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Returns a Service instsance
     * Notes:
     * @param poBusinessTask The reference to the IBusinessTaskCatalogData
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @param poDatasets The datasets to add to the catalog
     * @return The JsonObject representing the service
     */
    METHOD PROTECTED JsonObject GetService (poBusinessTask AS IBusinessTaskCatalogData,
                                            poAddress AS RestServiceAddress,
                                            poDatasets AS HandleList):

        DEFINE VARIABLE oService    AS JsonObject NO-UNDO .
        DEFINE VARIABLE oResources  AS JsonArray  NO-UNDO .

        ASSIGN THIS-OBJECT:EntityName = poBusinessTask:BusinessTaskName .

        oService   = NEW JsonObject () .
        oResources = NEW JsonArray () .

        oService:Add ("name":U, poAddress:Name) .
        oService:Add ("address":U, poAddress:Address) .
        oService:Add ("useRequest":U, TRUE) .

        oService:Add ("resources":U, oResources) .

        oResources:Add (THIS-OBJECT:GetResource (poBusinessTask,
                                                 poDatasets)) .

        RETURN oService .

        FINALLY:
            ASSIGN THIS-OBJECT:EntityName = ? .
        END FINALLY.

    END METHOD .

END CLASS.
