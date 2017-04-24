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
    File        : SemanticTypesProvider
    Purpose     : Default service that provides semantic types to
                  the Business Entity Designer
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 02 00:40:11 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                                       FROM PROPATH .
USING Consultingwerk.Util.*                                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.* FROM PROPATH .
USING Progress.Json.ObjectModel.*                            FROM PROPATH .
USING Progress.Lang.*                                        FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.SemanticTypesProvider
    IMPLEMENTS ISemanticTypesProvider:

    DEFINE VARIABLE oSemanticTypesMap AS JsonObject NO-UNDO .
    DEFINE VARIABLE oDataTypesMap AS JsonObject NO-UNDO .

    /**
     * Purpose: Constructor for the SemanticTypesProvider class
     * Notes:
     */
    CONSTRUCTOR PUBLIC SemanticTypesProvider ():

        DEFINE VARIABLE oParser AS ObjectModelParser NO-UNDO .

        SUPER () .

        oParser = NEW ObjectModelParser() .
        oSemanticTypesMap = CAST (oParser:ParseFile (FileHelper:FindFile ("Consultingwerk/BusinessEntityDesigner/KendoUiBuilder/semantic-types-map.json":U)),
                                  JsonObject) .

        oDataTypesMap = CAST (oParser:ParseFile (FileHelper:FindFile ("Consultingwerk/BusinessEntityDesigner/KendoUiBuilder/abl-to-semantic-type-map.json":U)),
                              JsonObject) .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the Default Editor for the given Semantic Type
     * Notes:
     * @param pcSemanticType The semantic type to return the default editor for
     * @return The editor type for the semantic type
     */
    METHOD PUBLIC CHARACTER DefaultEditorForSemanticType (pcSemanticType AS CHARACTER):

        DEFINE VARIABLE oSemanticTypes AS JsonObject        NO-UNDO .

        oSemanticTypes = oSemanticTypesMap:GetJsonObject("semanticTypes":U) .

        IF oSemanticTypes:Has (pcSemanticType) THEN
            RETURN JsonHelper:GetJsonObjectByPath (oSemanticTypes, SUBSTITUTE ("&1~\editor":U, pcSemanticType)):GetCharacter ("default":U) .

        RETURN "text-input":U .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error:
            RETURN "text-input":U .
        END CATCH.

    END METHOD .

    /**
     * Purpose: Returns the default Semantic Type for the given ABL data type
     * Notes:
     * @param pcDataType The ABL data type to return the semantic type for
     * @return The semantic type
     */
    METHOD PUBLIC CHARACTER DefaultSemanticTypeForDataType (pcDataType AS CHARACTER):

        DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

        ASSIGN cName = JsonHelper:ExactPropertyName(oDataTypesMap, pcDataType) .

        IF cName > "":U THEN
            RETURN oDataTypesMap:GetCharacter(cName) .

        RETURN "Internal":U .

    END METHOD .

    /**
     * Purpose: Returns the list of Semantic Types to the caller
     * Notes:
     * @param pcDataType The Data Type of the Field
     * @return The List of NameValuePair's with the available semantic Types
     */
    METHOD PUBLIC ListNameValuePair GetSemanticTypes (pcDataType AS CHARACTER):

        DEFINE VARIABLE oList          AS ListNameValuePair NO-UNDO .
        DEFINE VARIABLE oSemanticTypes AS JsonObject        NO-UNDO .
        DEFINE VARIABLE cNames         AS CHARACTER         NO-UNDO EXTENT .
        DEFINE VARIABLE i              AS INTEGER           NO-UNDO .

        oSemanticTypes = oSemanticTypesMap:GetJsonObject("semanticTypes":U) .

        ASSIGN cNames = oSemanticTypes:GetNames() .

        oList = NEW ListNameValuePair () .

        DO i = 1 TO EXTENT (cNames):

            oList:Add (cNames[i], cNames[i]) .

        END .

        RETURN oList .

    END METHOD .

END CLASS.
