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
    File        : JsonHelper
    Purpose     : Helper class for handling Json Objects
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk
    Created     : Wed May 18 17:29:44 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Exceptions.*     FROM PROPATH .
USING Consultingwerk.Framework.Base.* FROM PROPATH.
USING Consultingwerk.Util.*           FROM PROPATH .
USING Progress.Json.ObjectModel.*     FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Util.JsonHelper:

    /**
     * Purpose: Protected default constructor
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED JsonHelper():

    END CONSTRUCTOR.


&IF DEFINED (AblReflection) NE 0 &THEN
    /**
     * Purpose: Adds the properties of the given ABL Object to the
     *          JSON Object
     * Notes:
     * @param poObject The ABL object to add properties from
     * @param poJson The JSON object to add properties to
     * @param pcExceptPropertyNames The property names to exclude from the result
     */
    METHOD PUBLIC STATIC VOID AddObjectPropertiesToJson (poObject AS Progress.Lang.Object,
                                                         poJson AS JsonObject,
                                                         pcExceptPropertyNames AS CHARACTER):

        DEFINE VARIABLE oClass        AS Progress.Lang.Class       NO-UNDO .
        DEFINE VARIABLE oProperties   AS Progress.Reflect.Property NO-UNDO EXTENT .
        DEFINE VARIABLE oProperty     AS Progress.Reflect.Property NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE cValue        AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE oValue        AS Progress.Lang.Object      NO-UNDO .
        DEFINE VARIABLE oValueJson    AS JsonObject                NO-UNDO .
        DEFINE VARIABLE oArray        AS JsonArray                 NO-UNDO .
        DEFINE VARIABLE oInstanceJson AS JsonObject                NO-UNDO .

        oClass = poObject:GetClass() .

        oProperties = oClass:GetProperties() .

        propertyLoop:
        DO i = 1 TO EXTENT (oProperties):

            ASSIGN
                oProperty  = oProperties[i]
                oValue     = ?
                oValueJson = ? .

            IF INDEX (pcExceptPropertyNames, oProperty:Name) > 0 THEN
                NEXT propertyLoop .

            CASE oProperty:DataType:
                WHEN Progress.Reflect.DataType:Object THEN DO ON ERROR UNDO, THROW:
                    oValue = oProperty:Get (poObject) .

                    IF VALID-OBJECT (oValue) THEN DO:
                        oValueJson = NEW JsonObject () .

                        JsonHelper:AddObjectPropertiesToJson (oValue, oValueJson, pcExceptPropertyNames) .

                        poJson:Add (oProperty:Name, oValueJson) .
                    END.
                    ELSE
                        cValue = "<invalid object reference>":U .

                    CATCH err AS Progress.Lang.Error :
                        cValue = SUBSTITUTE ("Error: &1":U, err:GetMessage (1)) .
                        poJson:Add (oProperty:Name, cValue) .
                    END CATCH.
                END .
                OTHERWISE DO ON ERROR UNDO, THROW:
                    cValue = STRING (oProperty:Get (poObject)) .

                    poJson:Add (oProperty:Name, cValue) .

                    CATCH err AS Progress.Lang.Error:
                        cValue = SUBSTITUTE ("Error: &1":U, err:GetMessage (1)) .
                        poJson:Add (oProperty:Name, cValue) .
                    END CATCH.
                END.
            END CASE .
        END.

        IF TYPE-OF (poObject, IEnumerable) THEN DO:

            ASSIGN oArray = NEW JsonArray () .

            poJson:Add ("entries":U, oArray) .

            {Consultingwerk/foreachABL.i Progress.Lang.Object oInstance IN poObject}

                oInstanceJson = NEW JsonObject () .

                oArray:Add (oInstanceJson) .

                IF TYPE-OF (oInstance, Consultingwerk.NameValuePair) THEN DO:
                    oInstanceJson:Add ("Name":U, CAST (oInstance, Consultingwerk.NameValuePair):Name) .
                    oInstanceJson:Add ("Value":U, CAST (oInstance, Consultingwerk.NameValuePair):Value) .
                END.
                ELSE
                    JsonHelper:AddObjectPropertiesToJson (oInstance,
                                                          oInstanceJson,
                                                          pcExceptPropertyNames).
            END .
        END.

    END METHOD.
&ENDIF

    /**
     * Purpose: Returns the property name if found in the JSON object in the exact case
     *          as it appears in the JSON object
     * Notes:
     * @param poJsonObject The JsonObject to search for the property in
     * @param pcPropertyName The property name
     * @return The exact property name as it appears int he JSON object
     */
    METHOD PUBLIC STATIC CHARACTER ExactPropertyName (poJsonObject AS JsonObject,
                                                      pcPropertyName AS CHARACTER):

        DEFINE VARIABLE iEntry           AS INTEGER    NO-UNDO .
        DEFINE VARIABLE cNames           AS CHARACTER  NO-UNDO .

        IF poJsonObject:Has (pcPropertyName) THEN
            RETURN pcPropertyName .

        ASSIGN cNames = ArrayHelper:ToList (poJsonObject:GetNames())
               iEntry = LOOKUP (pcPropertyName, cNames) .

        IF iEntry = 0 THEN
            RETURN ? .
        ELSE
            RETURN ENTRY (iEntry, cNames) /* exact casing as in JSON */.

    END METHOD.

&IF NOT PROVERSION BEGINS "10" AND NOT PROVERSION EQ "11.0" AND NOT PROVERSION EQ "11.1" AND NOT PROVERSION EQ "11.2" AND NOT PROVERSION EQ "11.3" &THEN
    /**
     * Purpose: Returns the JSON Object with the contents of the given ProDataset
     * Notes:   Workaround, as the Dataset'S WRITE-JSON method does not support
     *          writing to a JsonObject directly
     * @param phDataset The handle of the dataset to return as JSON
     * @return The resulting JsonObject
     */
    METHOD PUBLIC STATIC JsonObject FromDataset (phDataset AS HANDLE):

        DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO .

        oJsonObject = NEW JsonObject ().

        oJsonObject:Read (phDataset, FALSE, TRUE) .

        RETURN oJsonObject .

    END METHOD .
&ENDIF

    /**
     * Purpose: Returns the Json Object represented by a path in the source
     *          Json Object
     * Notes:   The path entries can be delimited by either period (.), slash (/)
     *          or back-slash (\)
     *          Defaults to 1 based arrays (ABL default), Sample path:
     *          dsCustomer.eCustomer[1].eSalesRep[1]
     * @param poSourceJsonObject The source Json object that contains the target Json object
     * @param pcPath The path specifying the target object
     * @return The target Json object
     */
    METHOD PUBLIC STATIC JsonObject GetJsonObjectByPath (poSourceJsonObject AS JsonObject,
                                                         pcPath AS CHARACTER):

        RETURN JsonHelper:GetJsonObjectByPath (poSourceJsonObject, pcPath, ArrayBaseEnum:One) .

    END METHOD.

    /**
     * Purpose: Returns the Json Object represented by a path in the source
     *          Json Object
     * Notes:   The path entries can be delimited by either period (.), slash (/)
     *          or back-slash (\)
     *          dsCustomer.eCustomer[1].eSalesRep[1]
     * @param poSourceJsonObject The source Json object that contains the target Json object
     * @param pcPath The path specifying the target object
     * @param poArrayBase The array base (one or zero)
     * @return The target Json object
     */
    METHOD PUBLIC STATIC JsonObject GetJsonObjectByPath (poSourceJsonObject AS JsonObject,
                                                         pcPath AS CHARACTER,
                                                         poArrayBase AS ArrayBaseEnum):

        DEFINE VARIABLE i           AS INTEGER    NO-UNDO .
        DEFINE VARIABLE cEntry      AS CHARACTER  NO-UNDO .
        DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO .
        DEFINE VARIABLE iIndex      AS INTEGER    NO-UNDO .

        ASSIGN pcPath      = REPLACE (pcPath, "/":U, ".":U)
               pcPath      = REPLACE (pcPath, "~\":U, ".":U)

               oJsonObject = poSourceJsonObject .

        DO i = 1 TO NUM-ENTRIES (pcPath, ".":U):

            ASSIGN cEntry = ENTRY (i, pcPath, ".":U) .

            IF INDEX (cEntry, "[":U) > 0 THEN DO:

                ASSIGN iIndex = DataTypeHelper:ToInteger(TRIM (SUBSTRING (cEntry, INDEX (cEntry, "[":U) + 1), "]":U))
                       cEntry = SUBSTRING (cEntry, 1, INDEX (cEntry, "[":U) - 1) .

                IF NOT oJsonObject:Has (cEntry) THEN
                    ASSIGN cEntry = JsonHelper:ExactPropertyName (oJsonObject, cEntry) .

                IF cEntry = ? THEN
                    UNDO, THROW NEW InvalidParameterValueException ("pcPath":U,
                                                                    "Consultingwerk.Util.JsonHelper":U,
                                                                    SUBSTITUTE ("Invalid entry &1 in path &2."{&TRAN},
                                                                                ENTRY (i, pcPath, ".":U),
                                                                                pcPath),
                                                                    0) .

                IF iIndex >= 0 THEN DO:
                    IF poArrayBase = ArrayBaseEnum:Zero THEN
                        ASSIGN iIndex = iIndex + 1 .

                    ASSIGN oJsonObject = oJsonObject:GetJsonArray (cEntry):GetJsonObject (iIndex) .
                END.
                ELSE
                    UNDO, THROW NEW InvalidParameterValueException ("pcPath":U,
                                                                    "Consultingwerk.Util.JsonHelper":U,
                                                                    SUBSTITUTE ("Invalid entry &1 in path &2."{&TRAN},
                                                                                ENTRY (i, pcPath, ".":U),
                                                                                pcPath),
                                                                    0) .

            END.
            ELSE DO:
                IF NOT oJsonObject:Has (cEntry) THEN
                    ASSIGN cEntry = JsonHelper:ExactPropertyName (oJsonObject, cEntry) .

                IF cEntry = ? THEN
                    UNDO, THROW NEW InvalidParameterValueException ("pcPath":U,
                                                                    "Consultingwerk.Util.JsonHelper":U,
                                                                    SUBSTITUTE ("Invalid entry &1 in path &2."{&TRAN},
                                                                                ENTRY (i, pcPath, ".":U),
                                                                                pcPath),
                                                                    0) .

                oJsonObject = oJsonObject:GetJsonObject (cEntry) .
            END.
        END.

        RETURN oJsonObject .

    END METHOD.

    /**
     * Purpose: Returns the CHARACTER representation of the given JsonObject
     * Notes:   Truncates the JSON String to 20000 characters
     * @param poJsonObject The JsonObject to return the longchar representation of
     * @return The Character representation of the given string
     */
    METHOD PUBLIC STATIC CHARACTER ToCharacter (poJsonObject AS JsonObject):

        DEFINE VARIABLE lcJsonString AS LONGCHAR NO-UNDO .

        FIX-CODEPAGE (lcJsonString) = Codepages:UTF-8 .

        poJsonObject:Write (INPUT-OUTPUT lcJsonString, TRUE) .

        RETURN STRING (SUBSTRING (lcJsonString, 1, 20000)) .

    END METHOD.

    /**
     * Purpose: Returns the LONGCHAR representation of the given JsonObject
     * Notes:
     * @param poJsonObject The JsonObject to return the longchar representation of
     * @return The Longchar representation of the given string
     */
    METHOD PUBLIC STATIC LONGCHAR ToLongchar (poJsonObject AS JsonObject):

        DEFINE VARIABLE lcJsonString AS LONGCHAR NO-UNDO .

        FIX-CODEPAGE (lcJsonString) = Codepages:UTF-8 .

        poJsonObject:Write (INPUT-OUTPUT lcJsonString, TRUE) .

        RETURN lcJsonString .

    END METHOD.

END CLASS.
