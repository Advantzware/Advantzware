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
    File        : TempTableSchemaExporter
    Purpose     : Exports Temp-Table Schema as a JSON String
    Syntax      :
    Description : Supports JSON Catalog Version 1.0
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 27 21:59:57 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*              FROM PROPATH .
USING Consultingwerk.Exceptions.*             FROM PROPATH .
USING Consultingwerk.Framework.*              FROM PROPATH .
USING Consultingwerk.Framework.Session.*      FROM PROPATH .
USING Consultingwerk.OERA.Schema.*            FROM PROPATH .
USING Consultingwerk.Util.*                   FROM PROPATH .
USING Progress.Json.ObjectModel.*             FROM PROPATH .
USING Progress.Lang.*                         FROM PROPATH .

CLASS Consultingwerk.OERA.OpenAccess.TempTableSchemaExporter:

    DEFINE PRIVATE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .
    DEFINE PRIVATE VARIABLE cTranslationScope      AS CHARACTER              NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the "_errorString": {"type": "string"} should be added
                 to the schema
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IncludeErrorString AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Converts the given ABL Data Type into a Json Catalog type
        Notes:
        @param pcAblType The ABL Data Type
        @return The Json Catalog data type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER AblTypeToCatalogType (pcAblType AS CHARACTER):

        CASE pcAblType:
            WHEN "CHARACTER":U THEN
                RETURN "string":U .
            WHEN "INTEGER":U OR WHEN "INT64":U OR WHEN "RECID":U THEN
                RETURN "integer":U .
            WHEN "LOGICAL":U THEN
                RETURN "boolean":U .
            WHEN "DATE":U OR WHEN "DATETIME":U OR WHEN "DATETIME-TZ":U THEN
                RETURN "string":U .
            WHEN "DECIMAL":U THEN
                RETURN "number":U .
            WHEN "BLOB":u THEN
                RETURN "string":U .
            WHEN "CLOB":U THEN
                RETURN "string":U .
            WHEN "RAW":U THEN
                RETURN "string":U .
            WHEN "ROWID":U THEN
                RETURN "string":U .
        END CASE .

        UNDO, THROW NEW InvalidValueException (pcAblType, "pcAblType":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds the temp-table field to the given JsonObject
        Notes:
        @param phField The handle of the temp-table field
        @param poJsonObject The JsonObject to add the field to
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddBufferField (phField AS HANDLE,
                                          poJsonObject AS JsonObject):

        DEFINE VARIABLE oField                 AS JsonObject NO-UNDO .
        DEFINE VARIABLE oItems                 AS JsonObject NO-UNDO .

        DEFINE VARIABLE cColumnLabelTranslated AS CHARACTER  NO-UNDO .

        oField = NEW JsonObject () .

        poJsonObject:Add (BufferHelper:FieldSerializeName (phField), oField) .

        IF phField:EXTENT > 1 THEN DO:
            oField:Add ("type":U, "array":U) .
            oField:Add ("maxItems":U, phField:EXTENT) .

            oItems = NEW JsonObject () .
            oItems:Add ("type":U, THIS-OBJECT:AblTypeToCatalogType (phField:DATA-TYPE)) .

            oField:Add ("items":U, oItems) .
        END.
           ELSE
            oField:Add ("type":U, THIS-OBJECT:AblTypeToCatalogType (phField:DATA-TYPE)) .

        oField:Add ("ablType":U, UPPER (phField:DATA-TYPE)) .

        IF phField:DATA-TYPE = "BLOB":U OR phField:DATA-TYPE = "RAW":U OR phField:DATA-TYPE = "ROWID":U THEN
            oField:Add ("contentEncoding":U, "base64":U) .

        CASE phField:DATA-TYPE:
            WHEN "CHARACTER":U OR WHEN "CLOB":U THEN
                oField:Add ("default":U, phField:DEFAULT-STRING) .
            WHEN "BLOB":U OR WHEN "RAW":U THEN
                oField:AddNull ("default":U) .
            WHEN "DECIMAL":U THEN
                oField:Add ("default":U, DECIMAL (phField:DEFAULT-STRING)) .
            WHEN "INTEGER":U THEN
                oField:Add ("default":U, INTEGER (phField:DEFAULT-STRING)) .
            WHEN "INT64":U THEN
                oField:Add ("default":U, INT64 (phField:DEFAULT-STRING)) .
            WHEN "LOGICAL":U THEN
                oField:Add ("default":U, LOGICAL (phField:DEFAULT-STRING)) .
            WHEN "DATE":U THEN DO:
                oField:Add ("default":U, DATE (phField:DEFAULT-STRING)) .
                oField:Add ("format":U, "date":U) .
            END .
            WHEN "DATETIME":U THEN DO:
                oField:Add ("default":U, DATETIME (phField:DEFAULT-STRING)) .
                oField:Add ("format":U, "date-time":U) .
            END.
            WHEN "DATETIME-TZ":U THEN DO:
                oField:Add ("default":U, DATETIME-TZ (phField:DEFAULT-STRING)) .
                oField:Add ("format":U, "date-time":U) .
            END.
            WHEN "RECID":U THEN DO:
                oField:Add ("default":U, 0) .
            END.
            WHEN "ROWID":U THEN DO:
                oField:AddNull ("default":U) .
            END.
        END CASE .

        IF cTranslationScope > "":U THEN
            ASSIGN cColumnLabelTranslated = {Consultingwerk/translate.i &scope=cTranslationScope &key="SUBSTITUTE ('&1.&2.Label':U, phField:BUFFER-HANDLE:NAME, phField:NAME)" &defaulttext="phField:LABEL"} .

        IF cColumnLabelTranslated > "" THEN
            oField:Add ("title":U, cColumnLabelTranslated) .
        ELSE IF phField:LABEL > "":U THEN
            oField:Add ("title":U, phField:LABEL) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds the _id node to the properties (fields) of a table
        Notes:
        @param poJsonObject as JsonObject
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddIdField (poJsonObject AS JsonObject):

        DEFINE VARIABLE oId AS JsonObject NO-UNDO .

        oId = NEW JsonObject () .

        oId:Add ("type":U, "string":U) .

        poJsonObject:Add ("_id":U, oId) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds the _errorString node to the properties (fields) of a table
        Notes:
        @param poJsonObject as JsonObject
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddErrorStringField (poJsonObject AS JsonObject):

        DEFINE VARIABLE oId AS JsonObject NO-UNDO .

        oId = NEW JsonObject () .

        oId:Add ("type":U, "string":U) .

        poJsonObject:Add ("_errorString":U, oId) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds nodes for the schema of the given temp-table to a new JsonObject
        Notes:
        @param phBuffer The handle of the temp-table buffer
        @return The reference to the JsonObject
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC JsonObject TempTableSchemaToJsonObject (phBuffer AS HANDLE):

        DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO .

        oJsonObject = NEW JsonObject () .

        THIS-OBJECT:TempTableSchemaToJsonObject (phBuffer, "array":U, oJsonObject) .

        RETURN oJsonObject .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds nodes for the schema of the given temp-table to the given
                 JsonObject
        Notes:
        @param phBuffer The handle of the temp-table buffer
        @param poJsonObject The reference to the JsonObject
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID TempTableSchemaToJsonObject (phBuffer AS HANDLE,
                                                    poJsonObject AS JsonObject):

        THIS-OBJECT:TempTableSchemaToJsonObject (phBuffer, "array":U, poJsonObject) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds nodes for the schema of the given temp-table to the given
                 JsonObject
        Notes:
        @param phBuffer The handle of the temp-table buffer
        @param pcType The type attribute for the table reference (array / object)
        @param poJsonObject The reference to the JsonObject
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID TempTableSchemaToJsonObject (phBuffer AS HANDLE,
                                                    pcType AS CHARACTER,
                                                    poJsonObject AS JsonObject):

        DEFINE VARIABLE oTableObject           AS JsonObject             NO-UNDO .
        DEFINE VARIABLE oItemsObject           AS JsonObject             NO-UNDO .
        DEFINE VARIABLE oProperties            AS JsonObject             NO-UNDO .

        DEFINE VARIABLE hField                 AS HANDLE                 NO-UNDO .
        DEFINE VARIABLE i                      AS INTEGER                NO-UNDO .

        BufferAssert:IsTempTableBuffer (phBuffer) .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poJsonObject """poJsonObject"":U"} .

        /* Mike Fechner, Consultingwerk Ltd. 20.03.2016
           SCL-1254: Translation of Column Lables, requires known language of the user.
                     The language should be provided by a handle of the ServiceInterface:Activate
                     event */
        IF SessionManager:LanguageKey > "":U THEN DO:
            ASSIGN oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

            IF VALID-OBJECT (oConfigurationProvider) THEN
                ASSIGN cTranslationScope = oConfigurationProvider:GetValue ("WebLocalizationScope":U) .
        END.

        oTableObject = NEW JsonObject () .
        oItemsObject = NEW JsonObject () .
        oProperties  = NEW JsonObject () .

        poJsonObject:Add (phBuffer:NAME, oTableObject) .

        oTableObject:Add ("type":U, pcType) .
        oTableObject:Add ("items":U, oItemsObject) .

        oItemsObject:Add ("additionalProperties":U, FALSE) .
        oItemsObject:Add ("properties":U, oProperties) .

        THIS-OBJECT:AddIdField (oProperties) .

        IF THIS-OBJECT:IncludeErrorString THEN
            THIS-OBJECT:AddErrorStringField (oProperties) .

        DO i = 1 TO phBuffer:NUM-FIELDS:
            ASSIGN hField = phBuffer:BUFFER-FIELD (i) .

            THIS-OBJECT:AddBufferField (hField, oProperties) .
        END.

        FINALLY:
            ASSIGN oConfigurationProvider = ?
                   cTranslationScope      = ? .
        END FINALLY.

    END METHOD .

END CLASS.
