/**************************************************************alse********
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
    File        : CatalogV14
    Purpose     : Generic JSON/JSDO Catalog generation, V1.4 (OpenEdge 11.6.2)
                  with Kendo UI Builder extension (Star*Nova)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Aug 04 21:03:02 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.*       FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Progress.Json.ObjectModel.*                    FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.OERA.JsdoGenericService.CatalogV14
    INHERITS CatalogV12:

    /**
     * Purpose: Constructor for the CatalogV14 class
     * Notes:
     */
    CONSTRUCTOR PUBLIC CatalogV14():

        ASSIGN THIS-OBJECT:CatalogVersion = "1.4":U .

    END CONSTRUCTOR.

    /**
     * Purpose: Adds a boolean attribute to all table columns based on
     *          a list of columns
     * Notes:   Used for ReadOnly, Mandatory, non filterable and non sortable
     *          Columns
     * @param poProperties The JsonObject representing the columns of a table in the catalog
     * @param pcAttributeName The attribute name to attach to the column
     * @param pcColumns The comma delimited list of columns to add the attribute to
     */
    METHOD PROTECTED VOID AddColumnAttributes (poProperties AS JsonObject,
                                               pcAttributeName AS CHARACTER,
                                               pcColumns AS CHARACTER):

        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cColumnName AS CHARACTER NO-UNDO .

        DO i = 1 TO NUM-ENTRIES (pcColumns):
            ASSIGN cColumnName = JsonHelper:ExactPropertyName (poProperties, ENTRY (i, pcColumns)) .

            IF cColumnName > "":U THEN
                poProperties:GetJsonObject (cColumnName):Add (pcAttributeName, TRUE) .
        END.

    END METHOD.

    /**
     * Purpose: Returns the JSON Schema description for the ProDataset
     * Notes:
     *
     * @param phDataset The ProDataset to return the schema for
     * @return @return The JsonObject representing the schema
     */
    METHOD PUBLIC OVERRIDE JsonObject GetSchema (phDataset AS HANDLE):

        DEFINE VARIABLE oSchema                   AS JsonObject                           NO-UNDO .
        DEFINE VARIABLE oDataset                  AS JsonObject                           NO-UNDO .
        DEFINE VARIABLE oTable                    AS JsonObject                           NO-UNDO .
        DEFINE VARIABLE oField                    AS JsonObject                           NO-UNDO .
        DEFINE VARIABLE oProperties               AS JsonObject                           NO-UNDO .
        DEFINE VARIABLE i                         AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE oBusinessEntityDescriptor AS IBusinessEntityDescriptor            NO-UNDO .
        DEFINE VARIABLE oTableDescriptor          AS IBusinessEntityTableDescriptor       NO-UNDO .
        DEFINE VARIABLE oParameter                AS GetBusinessEntityDescriptorParameter NO-UNDO .
        DEFINE VARIABLE hDataset                  AS HANDLE                               NO-UNDO .

        DEFINE VARIABLE cTables                   AS CHARACTER                            NO-UNDO EXTENT .
        DEFINE VARIABLE cPrimaryKey               AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cReadOnlyColumns          AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cMandatoryColumns         AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cNonFilterableColumns     AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cNonSortableColumns       AS CHARACTER                            NO-UNDO .

        oParameter = NEW GetBusinessEntityDescriptorParameter (FALSE) .

        ServiceInterface:InvokeMethod (THIS-OBJECT:EntityName,
                                      "GetBusinessEntityDescriptor":U,
                                      INPUT-OUTPUT DATASET-HANDLE phDataset,
                                      oParameter) .

        oBusinessEntityDescriptor = oParameter:Descriptor .

        oSchema = SUPER:GetSchema (phDataset) .

        oDataset = oSchema:GetJsonObject ("properties":U):GetJsonObject (phDataset:NAME):GetJsonObject ("properties":U) .

        ASSIGN cTables = oDataset:GetNames () .

        DO i = 1 TO EXTENT (cTables):

            ASSIGN oTable      = oDataset:GetJsonObject(cTables[i])
                   oProperties = oTable:GetJsonObject("items":U):GetJsonObject("properties":U) .

            IF oProperties:Has ("_id":U) THEN
                oProperties:GetJsonObject("_id":U):Add ("semanticType":U, "Internal":U) .

            IF oProperties:Has ("_errorString":U) THEN
                oProperties:GetJsonObject("_errorString":U):Add ("semanticType":U, "Internal":U) .

            IF oProperties:Has ("SmartRecordKey":U) THEN
                oProperties:GetJsonObject("SmartRecordKey":U):Add ("semanticType":U, "Internal":U) .

            IF oProperties:Has ("SmartAttachments":U) THEN
                oProperties:GetJsonObject("SmartAttachments":U):Add ("semanticType":U, "Internal":U) .

            IF oProperties:Has ("SmartComments":U) THEN
                oProperties:GetJsonObject("SmartComments":U):Add ("semanticType":U, "Internal":U) .

            IF oProperties:Has ("SmartCopiedFrom":U) THEN
                oProperties:GetJsonObject("SmartCopiedFrom":U):Add ("semanticType":U, "Internal":U) .

            IF oProperties:Has ("id":U) THEN
                oProperties:GetJsonObject("id":U):Add ("semanticType":U, "Internal":U) .

            IF oProperties:Has ("seq":U) THEN
                oProperties:GetJsonObject("seq":U):Add ("semanticType":U, "Internal":U) .

            IF oBusinessEntityDescriptor:Tables:ContainsName (cTables[i]) THEN DO:

                ASSIGN oTableDescriptor      = oBusinessEntityDescriptor:Tables:GetItem (cTables[i])
                       cPrimaryKey           = oTableDescriptor:PrimaryKey

                       cReadOnlyColumns      = oTableDescriptor:ReadOnlyColumns
                       cMandatoryColumns     = oTableDescriptor:MandatoryColumns
                       cNonFilterableColumns = oTableDescriptor:NonFilterableColumns
                       cNonSortableColumns   = oTableDescriptor:NonSortableColumns .

                IF cPrimaryKey > "":U THEN DO:

                    oTable:Add ("primaryKey":U, NEW JsonArray (ArrayHelper:ListToCharacterArray(cPrimaryKey, ",":U))) .

                END.

                IF cReadOnlyColumns > "":U THEN
                    THIS-OBJECT:AddColumnAttributes (oProperties, "readOnly":U, cReadOnlyColumns) .

                IF cMandatoryColumns > "":U THEN
                    THIS-OBJECT:AddColumnAttributes (oProperties, "mandatory":U, cMandatoryColumns) .

                IF cNonFilterableColumns > "":U THEN
                    THIS-OBJECT:AddColumnAttributes (oProperties, "nonFilterable":U, cNonFilterableColumns) .

                IF cNonSortableColumns > "":U THEN
                    THIS-OBJECT:AddColumnAttributes (oProperties, "nonSortable":U, cNonSortableColumns) .

                {Consultingwerk/foreachABL.i IBusinessEntityFieldDescriptor oFieldDescriptor IN oTableDescriptor:Fields}

                    IF oFieldDescriptor:SemanticType > "":U THEN DO:
                        IF oProperties:Has(oFieldDescriptor:Name) THEN DO:
                            oField = oProperties:GetJsonObject(oFieldDescriptor:Name) .

                            oField:Add ("semanticType":U, oFieldDescriptor:SemanticType) .
                        END .
                    END.
                END.
            END.
        END.

        RETURN oSchema .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD.

END CLASS.
