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
    File        : DataProviderGenerator
    Purpose     : Generator for Kendo UI Builder for OpenEdge Data Provider
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 30 09:15:25 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                       FROM PROPATH.
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.* FROM PROPATH.
USING Consultingwerk.Util.*                                  FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.DataProvider.*       FROM PROPATH .
USING Progress.Json.*                                        FROM PROPATH.
USING Progress.Json.ObjectModel.*                            FROM PROPATH .
USING Progress.Lang.*                                        FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.DataProviderGenerator:

    {Consultingwerk/BusinessEntityDesigner/KendoUiBuilder/ttDataSources.i}
    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i}

    /**
     * Purpose: Constructor for the DataSourceGenerator class
     * Notes:
     */
    CONSTRUCTOR PUBLIC DataProviderGenerator ():

        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the file name the generator is writing to
     * Notes:
     * @param pcName The name of the Data Provider to generate
     * @param pcAppLocation The folder of the Kendo UI Builder builder project
     * @return The file name
     */
    METHOD PUBLIC CHARACTER BuildFileName (pcName                 AS CHARACTER,
                                           pcAppLocation          AS CHARACTER):

        RETURN SUBSTITUTE ("&1~\meta~\dataProviders~\&2.json":U,
                           pcAppLocation,
                           pcName) .

    END METHOD.

    /**
     * Purpose: Returns if the file to generate already exists
     * Notes:
     * @param pcName The name of the Data Provider to generate
     * @param pcAppLocation The folder of the Kendo UI Builder builder project
     * @return Logical value indicating if the file already exists
     */
    METHOD PUBLIC LOGICAL FileAlreadyExists (pcName                 AS CHARACTER,
                                             pcAppLocation          AS CHARACTER):

        RETURN FileHelper:Exists (THIS-OBJECT:BuildFileName(pcName, pcAppLocation)).

    END METHOD.

    /**
     * Purpose: Generates a Kendo UI Data Source
     * Notes:
     * @param pcName The name of the Data Provider to generate
     * @param pcServiceURI The Service URI
     * @param pcCatalogURI The Catalog URI
     * @param pcCatalogUriEntityName The Entity Name for the Catalog URI
     * @param pcAuthenticationModel The Authentication Model
     * @param pcAppLocation The folder of the Kendo UI Builder builder project
     * @param pcKendoUiBuilderUri The URI of the Kendo UI Builder development environment
     * @param ttDataSources The temp-table with the data source definitions
     * @param dsBusinessEntity The dataset with the Business Entity definition
     * @return The file name of the generated JSON file
     */
    METHOD PUBLIC CHARACTER GenerateDataProvider (pcName                 AS CHARACTER,
                                                  pcServiceURI           AS CHARACTER,
                                                  pcCatalogURI           AS CHARACTER,
                                                  pcCatalogUriEntityName AS CHARACTER,
                                                  pcAuthenticationModel  AS CHARACTER,
                                                  pcAppLocation          AS CHARACTER,
                                                  pcKendoUiBuilderUri    AS CHARACTER,
                                                  TABLE ttDataSources,
                                                  DATASET dsBusinessEntity):

        DEFINE VARIABLE cFileName             AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE oDataProvider         AS DataProvider           NO-UNDO .
        DEFINE VARIABLE oDataSource           AS DataSource             NO-UNDO .
        DEFINE VARIABLE oRelation             AS Relation               NO-UNDO .
        DEFINE VARIABLE cSemanticType         AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE oSemanticTypeProvider AS ISemanticTypesProvider NO-UNDO .
        DEFINE VARIABLE cEditorType           AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE i                     AS INTEGER                NO-UNDO .

        DEFINE VARIABLE oJson          AS JsonObject   NO-UNDO .

        oSemanticTypeProvider = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.ISemanticTypesProvider
                                                              "NEW Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.SemanticTypesProvider ()"} .

        ASSIGN cFileName = THIS-OBJECT:BuildFileName(pcName, pcAppLocation)

               oDataProvider = NEW DataProvider (pcName)

               oDataProvider:serviceUri = pcServiceURI
               oDataProvider:catalogUris = ArrayHelper:Array (SUBSTITUTE ("&1/&2":U ,
                                                                          pcCatalogURI,
                                                                          pcCatalogUriEntityName))
               oDataProvider:autoGenerateDataSources = FALSE
               oDataProvider:authenticationModel     = CAST (EnumHelperExt:FromString (pcAuthenticationModel,
                                                                                       Progress.Lang.Class:GetClass ("Consultingwerk.Web.KendoUiBuilder.DataProvider.authenticationModelEnum":U)),
                                                             authenticationModelEnum)
               .

        FOR EACH ttDataSources WHERE ttDataSources.GenerateDataSource BY ttDataSources.Sequence:

            oDataSource = oDataProvider:children:Add (ttDataSources.DataSourceName) .

            ASSIGN oDataSource:resourceName = pcCatalogUriEntityName
                   oDataSource:serviceName  = "web-":U + pcCatalogUriEntityName
                   oDataSource:tableName    = ttDataSources.DataSourceName .

            FOR EACH eField WHERE eField.TempTableName = ttDataSources.TableName
                               BY eField.FieldOrder:

                IF CharacterType:IsNullOrEmpty (eField.FieldSemanticType) THEN DO:
                    IF ListHelper:EntryIsInList(eField.FieldName, "SmartRecordKey,SmartAttachments,SmartComments,SmartCopiedFrom":U) THEN
                        ASSIGN cSemanticType = "Internal":U .
                    ELSE
                        ASSIGN cSemanticType = oSemanticTypeProvider:DefaultSemanticTypeForDataType(eField.FieldDataType) .
                END.
                ELSE
                    ASSIGN cSemanticType = eField.FieldSemanticType .

                ASSIGN cEditorType = oSemanticTypeProvider:DefaultEditorForSemanticType (cSemanticType) .

                IF cEditorType <> ? THEN
                    /*oField =*/ oDataSource:fields:Add (eField.FieldName,
                                                         eField.FieldLabel,
                                                         cEditorType,
                                                         cSemanticType,
                                                         "") .
            END.
                /*oField =*/ oDataSource:fields:Add ("id":U,
                                                     "id":U,
                                                     ?,
                                                     "Internal":U,
                                                     "") .

                /*oField =*/ oDataSource:fields:Add ("seq":U,
                                                     "seq":U,
                                                     ?,
                                                     "Internal":U,
                                                     "") .

            FOR EACH eDataRelation WHERE eDataRelation.ParentTempTableName = ttDataSources.TableName
                                      OR eDataRelation.ChildTempTableName  = ttDataSources.TableName:

                oRelation = oDataSource:relations:Add (eDataRelation.DataRelationName,
                                                       eDataRelation.ParentTempTableName,
                                                       eDataRelation.ChildTempTableName) .

                DO i = 1 TO NUM-ENTRIES (eDataRelation.RelationFields) BY 2:
                    oRelation:relationFields:Add (ENTRY (i, eDataRelation.RelationFields),
                                                  ENTRY (i + 1, eDataRelation.RelationFields)) .
                END.
            END.
        END .

        oJson = oDataProvider:ToJsonObject() .

        oJson:WriteFile (cFileName, TRUE) .

        RETURN cFileName .

    END METHOD.

END CLASS.
