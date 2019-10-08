/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Grid
    Purpose     : Returns the json fragment for a Kendo UI grid of
                  SmartComponents.Web2
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 11:04:20 CEST 2015
    Notes       : Supports Business Entity Descriptor defined views:
                  "Consultingwerk.SmartComponentsDemo.OERA.Sports2000.CustomerBusinessEntity#customer"
  ----------------------------------------------------------------------*/

@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Assertion.*                     FROM PROPATH .
USING Consultingwerk.Exceptions.*                    FROM PROPATH .
USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.Framework.Authorization.*       FROM PROPATH .
USING Consultingwerk.Framework.Session.*             FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Consultingwerk.Web.Kendo.Grid.*                FROM PROPATH .
USING Consultingwerk.Web2.Services.Exceptions.*      FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*      FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .
USING Progress.Json.ObjectModel.*                    FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.Services.SmartViews.Grid
    IMPLEMENTS IGridLayoutProvider:

    DEFINE TEMP-TABLE ttCustomizationProvider NO-UNDO
        FIELD EntityNamePattern     AS CHARACTER
        FIELD ViewNamePattern       AS CHARACTER
        FIELD CustomizationProvider AS Progress.Lang.Object
        INDEX EntityName EntityNamePattern ViewNamePattern .

    DEFINE TEMP-TABLE ttGridCustomProvider NO-UNDO
        FIELD CustomViewNamePattern AS CHARACTER
        FIELD CustomProvider AS Progress.Lang.Object
        INDEX CustomViewNamePattern CustomViewNamePattern .

    /**
     * Purpose: Constructor for the Grid class
     * Notes:
     */
    CONSTRUCTOR PUBLIC Grid ():

        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the HTML Fragment for the SmartGrid
     * Notes:
     * @param pcEntityName The name of the Business Entity to return a view spec form
     * @param pcViewName The name of the view
     * @param pcCode OUTPUT The html fragment
     */
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    METHOD PUBLIC VOID GetFragment (pcEntityName AS CHARACTER,
                                    pcViewName AS CHARACTER,
                                    OUTPUT pcCode AS LONGCHAR):

        pcCode = THIS-OBJECT:GetKendoGrid (pcEntityName,
                                           pcViewName,
                                           TRUE,
                                           SUBSTITUTE ("&1/&2":U, pcEntityName, pcViewName)):Serialize () .

    END METHOD .

    /**
     * Purpose: Returns the Json Object describing the Grid
     * Notes:
     * @param pcCustomViewName The name of the view
     * @return The JSON Object describing the Grid
     */
    METHOD PUBLIC JsonObject GetFragment (pcCustomViewName AS CHARACTER):

        DEFINE VARIABLE oGridCustomProvider AS IGridCustomProvider NO-UNDO .
        DEFINE VARIABLE oJsonObject         AS JsonObject          NO-UNDO .

        FOR EACH ttGridCustomProvider WHERE pcCustomViewName MATCHES ttGridCustomProvider.CustomViewNamePattern
                                           ON ERROR UNDO, THROW:

            ASSIGN oGridCustomProvider = CAST (ttGridCustomProvider.CustomProvider, IGridCustomProvider) .

            IF VALID-OBJECT (oGridCustomProvider) THEN DO:
                oJsonObject = oGridCustomProvider:GetFragment (pcCustomViewName) .

                IF VALID-OBJECT (oJsonObject) THEN
                    RETURN oJsonObject .
            END.
        END.

        UNDO, THROW NEW Exception (SUBSTITUTE ("Unsupported custom view name: &1"{&TRAN},
                                               pcCustomViewName), 0) .

    END METHOD .

    /**
     * Purpose: Returns the Json Object describing the Grid
     * Notes:
     * @param pcEntityName The name of the Business Entity to return a view spec form
     * @param pcViewName The name of the view
     * @return The JSON Object describing the Grid
     */
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    METHOD PUBLIC JsonObject GetFragment (pcEntityName AS CHARACTER,
                                          pcViewName AS CHARACTER):

        DEFINE VARIABLE oKendoGrid             AS KendoGrid                  NO-UNDO .
        DEFINE VARIABLE oJsonObject            AS JsonObject                 NO-UNDO .
        DEFINE VARIABLE oCustomizationProvider AS IGridCustomizationProvider NO-UNDO .

        ASSIGN oKendoGrid = THIS-OBJECT:GetKendoGrid (pcEntityName,
                                                      pcViewName,
                                                      TRUE,
                                                      SUBSTITUTE ("&1/&2":U, pcEntityName, pcViewName)) .

        FOR EACH ttCustomizationProvider WHERE pcEntityName MATCHES ttCustomizationProvider.EntityNamePattern
                                           AND pcViewName   MATCHES ttCustomizationProvider.ViewNamePattern
                                           ON ERROR UNDO, THROW:
            ASSIGN oCustomizationProvider = CAST (ttCustomizationProvider.CustomizationProvider, IGridCustomizationProvider) .

            IF VALID-OBJECT (oCustomizationProvider) THEN
                oCustomizationProvider:CustomizeGridPreSerialization (pcEntityName, pcViewName, oKendoGrid) .
        END.

        oJsonObject = oKendoGrid:ToJsonObject() .

        FOR EACH ttCustomizationProvider WHERE pcEntityName MATCHES ttCustomizationProvider.EntityNamePattern
                                           AND pcViewName   MATCHES ttCustomizationProvider.ViewNamePattern
                                           ON ERROR UNDO, THROW:
            ASSIGN oCustomizationProvider = CAST (ttCustomizationProvider.CustomizationProvider, IGridCustomizationProvider) .

            IF VALID-OBJECT (oCustomizationProvider) THEN
                oCustomizationProvider:CustomizeGridPostSerialization (pcEntityName, pcViewName, oJsonObject) .
        END.

        RETURN oJsonObject .

    END METHOD .

    /**
     * Purpose: Returns the HTML Fragment for the SmartGrid
     * Notes:
     * @param pcEntityName The name of the Business Entity to return a view spec form
     * @param pcViewName The name of the view
     * @param plPerformSecurityCheck Logical value indicating if the method should perform field level security checks (hidden, read-only)
     * @param pcSecurityKey The security key
     * @return The KendoGrid structure that defines the grid
     */
    METHOD PUBLIC KendoGrid GetKendoGrid (pcEntityName AS CHARACTER,
                                          pcViewName AS CHARACTER,
                                          plPerformSecurityCheck AS LOGICAL,
                                          pcSecurityKey AS CHARACTER):

        DEFINE VARIABLE hDataset                    AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oParameter                  AS GetBusinessEntityDescriptorParameter NO-UNDO .
        DEFINE VARIABLE oViewDescriptor             AS IBusinessEntityViewDescriptor        NO-UNDO .
        DEFINE VARIABLE i                           AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE cColumnName                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE oBufferFieldName            AS BufferFieldName                      NO-UNDO .
        DEFINE VARIABLE hBuffer                     AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE hBufferField                AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE cTables                     AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cFieldModel                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE oGrid                       AS KendoGrid                            NO-UNDO .
        DEFINE VARIABLE oColumn                     AS Column                               NO-UNDO .

        DEFINE VARIABLE oConfigurationProvider      AS IConfigurationProvider               NO-UNDO .
        DEFINE VARIABLE cTranslationScope           AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cColumnLabelTranslated      AS CHARACTER                            NO-UNDO .

        DEFINE VARIABLE oFieldAuthorizationProvider AS IFieldAuthorizationProvider          NO-UNDO .
        DEFINE VARIABLE oRestricted                 AS FieldRestrictionEnum                 NO-UNDO .
        DEFINE VARIABLE cSecurityField              AS CHARACTER                            NO-UNDO .

        Assert:NotNullOrEmpty (pcEntityName, "pcEntityName":U) .
        Assert:NotNullOrEmpty (pcViewName, "pcViewName":U) .

        ServiceInterface:Activate () .

        IF plPerformSecurityCheck THEN
            oFieldAuthorizationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.Authorization.IFieldAuthorizationProvider} .

        /* Mike Fechner, Consultingwerk Ltd. 20.03.2016
           SCL-1254: Translation of Column Lables, requires known language of the user.
                     The language should be provided by a handle of the ServiceInterface:Activate
                     event */
        IF SessionManager:LanguageKey > "":U THEN DO:
            ASSIGN oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

            IF VALID-OBJECT (oConfigurationProvider) THEN
                ASSIGN cTranslationScope = oConfigurationProvider:GetValue ("WebLocalizationScope":U) .
        END.

        oParameter = NEW GetBusinessEntityDescriptorParameter () .
        oParameter:ReturnDataset = TRUE .

        ServiceInterface:InvokeMethod (pcEntityName,
                                       "GetBusinessEntityDescriptor":U,
                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                       oParameter) .

        IF NOT oParameter:Descriptor:Views:ContainsName (pcViewName) THEN
            UNDO, THROW NEW Web2ServiceInterfaceException ("Invalid view name."{&TRAN}, 0) .

/*
{
                columns: [
                    { field: 'CustNum', title: 'Cust Num', width: 100 },
                    { field: 'Name' },
                    { field: 'State' },
                    { field: 'Country' },
                    { field: 'SalesRep' },
                    { field: 'RepName', title: 'Salesrep Name', sortable: false, filterable: false,
                        template: function (dataItem)
                                  {
                                    if ((dataItem.eSalesrep) && dataItem.eSalesrep.length > 0 )
                                    {
                                        return dataItem.eSalesrep[0].RepName;
                                    }
                                    else
                                        return "---";
                                  }
                    },

                    { command: ['edit', 'destroy'], title: '&nbsp;', width: '250px' }
                ]
            }

*/

        ASSIGN oViewDescriptor  = oParameter:Descriptor:Views:GetItem (pcViewName)
               cTables          = TRIM (SUBSTITUTE ("&1,&2":U, oViewDescriptor:EntityTable, oViewDescriptor:EntityView), ",":U)

               oGrid            = NEW KendoGrid ()
               oGrid:dataSource = ?  .

        /* Default grid properties */
        ASSIGN oGrid:height =  400
               oGrid:width = 600
               oGrid:groupable = FALSE
               oGrid:reorderable = TRUE
               oGrid:resizable = TRUE
               oGrid:sortable = TRUE
               oGrid:selectable = "row":U
               oGrid:filterable = NEW Filterable ("menu, row":U)
               oGrid:editable = NEW Editable ("inline":U)

               oGrid:pageable = NEW Consultingwerk.Web.Kendo.Grid.Pageable ()
               oGrid:pageable:refresh = TRUE
               oGrid:pageable:pageSizes = ArrayHelper:Array (10, 25, 100)
               oGrid:pageable:pageSize = 10 .
               oGrid:pageable:buttonCount = 5 .

        columnLoop:
        DO i = 1 TO NUM-ENTRIES (oViewDescriptor:ListColumns):

            ASSIGN cColumnName      = ENTRY (i, oViewDescriptor:ListColumns)
                   oBufferFieldName = BufferHelper:ParseFieldName (cColumnName) .

            IF oBufferFieldName:TableName > "":U THEN
                ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (oBufferFieldName:TableName) .
            ELSE
                ASSIGN hBuffer = DatasetHelper:FindBufferWithField (hDataset,
                                                                    cTables,
                                                                    oBufferFieldName:FieldName)  .

            IF NOT VALID-HANDLE (hBuffer) THEN
                UNDO, THROW NEW Web2ServiceInterfaceException (SUBSTITUTE ("Invalid buffer for field name &1."{&TRAN}, cColumnName), 0) .

            BufferAssert:HasField (hBuffer, oBufferFieldName:FieldName) .

            IF plPerformSecurityCheck AND VALID-OBJECT (oFieldAuthorizationProvider) THEN DO:
                ASSIGN cSecurityField = SUBSTITUTE ("&1.&2":U, hBuffer:NAME, oBufferFieldName:FieldName)
                       oRestricted = oFieldAuthorizationProvider:IsFieldRestricted (? /* Requestor is meaningless here */,
                                                                                    cSecurityField,
                                                                                    pcSecurityKey) .

                IF LogManager:IsCustomLogEntryTypeActive("FieldSecurity":U) THEN
                    LogManager:WriteFormattedMessage ("Field Security for &1 : &2":U,
                                                      cSecurityField,
                                                      oRestricted:ToString(),
                                                      NEW MessageSubSystem ("FieldSecurity":U)) .

                IF oRestricted = FieldRestrictionEnum:Hidden THEN
                    NEXT columnLoop .
            END.
            ELSE
                ASSIGN oRestricted = ? .

            ASSIGN hBufferField = hBuffer:BUFFER-FIELD (oBufferFieldName:FieldName) .

            IF hBuffer:NAME = oViewDescriptor:EntityTable THEN
                ASSIGN cFieldModel = BufferHelper:FieldSerializeName(hBufferField) .
            ELSE
                ASSIGN cFieldModel = SUBSTITUTE ("&1[0].&2":U, hBuffer:NAME, BufferHelper:FieldSerializeName(hBufferField)) .

            IF cTranslationScope > "":U THEN
                ASSIGN cColumnLabelTranslated = {Consultingwerk/translate.i &scope=cTranslationScope &key="SUBSTITUTE ('&1.&2.Label':U, hBuffer:NAME, hBufferField:NAME)" &defaulttext="hBufferField:LABEL"} .

            IF cColumnLabelTranslated > "" THEN
                oColumn = oGrid:Columns:Add (cFieldModel, cColumnLabelTranslated) .
            ELSE
                oColumn = oGrid:Columns:Add (cFieldModel, hBufferField:LABEL) .

            /* Default format for DATE and DATETIME/DATETIME-TZ columns */
            CASE hBufferField:DATA-TYPE:
                WHEN "DATE":U THEN
                    oColumn:format = "~{0:d}":U .
                WHEN "DATETIME":U OR WHEN "DATETIME-TZ":U THEN
                    oColumn:format = "~{0:g}":U .
            END CASE .

            IF hBuffer:NAME <> oViewDescriptor:EntityTable THEN

                ASSIGN oColumn:Template = SUBSTITUTE ("childRelation:&1:&2":U,
                                                      hBuffer:NAME,
                                                      hBufferField:NAME) .
        END.

        oColumn = oGrid:Columns:Add (NEW Column()) .
        oColumn:Command = ArrayHelper:Array ("destroy":U) .
        oColumn:title = "&nbsp;":U .
        oColumn:Width = "100px":U .

        RETURN oGrid .

        CATCH err AS Progress.Lang.Error:
            LogManager:WriteError (err) .

            UNDO, THROW err .
        END CATCH.

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .

            ServiceInterface:Deactivate () .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Registers an IGridCustomizationProvider
     * Notes:   Allows to customize a grid identified by the Business Entity Name and View Name
     * @param pcEntityNamePattern The name pattern (MATCHES) of Business Entities to customize
     * @param pcViewPattern The name pattern (MATCHES) of Views to customizat
     * @param poCustomizationProvider The customization provider instance
     * @return The IGridCustomizationProvider instance for fluent coding style
     */
    METHOD PUBLIC IGridCustomizationProvider RegisterCustomizationProvider (pcEntityNamePattern AS CHARACTER,
                                                                            pcViewPattern AS CHARACTER,
                                                                            poCustomizationProvider AS IGridCustomizationProvider):

        CREATE ttCustomizationProvider.
        ASSIGN ttCustomizationProvider.EntityNamePattern     = pcEntityNamePattern
               ttCustomizationProvider.ViewNamePattern       = pcViewPattern
               ttCustomizationProvider.CustomizationProvider = poCustomizationProvider .
        RELEASE ttCustomizationProvider.

        RETURN poCustomizationProvider .

    END METHOD .

    /**
     * Purpose: Registers an IGridCustomProvider
     * Notes:   Allows to return a custom grid layout to the browser independently
     *          of a Business Entity Name nad View Name
     * @param pcCustomViewPattern The name pattern (MATCHES) of Views to customize
     * @param poCustomProvider The customization provider instance
     * @return The IGridCustomProvider instance for fluent coding style
     */
    METHOD PUBLIC IGridCustomProvider RegisterGridCustomProvider (pcCustomViewPattern AS CHARACTER,
                                                                  poCustomProvider AS IGridCustomProvider):

        CREATE ttGridCustomProvider.
        ASSIGN ttGridCustomProvider.CustomViewNamePattern = pcCustomViewPattern
               ttGridCustomProvider.CustomProvider        = poCustomProvider .
        RELEASE ttGridCustomProvider.

        RETURN poCustomProvider .

    END METHOD .

END CLASS.
