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
    File        : ListValidationMessage
    Purpose     : List of ValidationMessage instances
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 27 09:49:35 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                         FROM PROPATH .
USING Consultingwerk.Framework.Base.*          FROM PROPATH .
USING Consultingwerk.OERA.ValidationMessages.* FROM PROPATH .
USING Consultingwerk.Util.*                    FROM PROPATH .
USING Progress.Json.ObjectModel.*              FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.OERA.ValidationMessages.ListValidationMessage
    INHERITS GenericList:

    { Consultingwerk/Framework/Base/GenericList.i Consultingwerk.OERA.ValidationMessages.ValidationMessage}

    /**
     * Purpose: Returns a new List instance with the messages filtered
     *          by the given field name
     * Notes:   Also applicable to the empty field name (messages of the record)
     * @param pcFieldName The field name to filter by
     * @return The ListValidationMessage with the messages filtered by the given field
     */
    METHOD PUBLIC ListValidationMessage FilterByField (pcFieldName AS CHARACTER):

        DEFINE VARIABLE oList    AS ListValidationMessage NO-UNDO .

        oList = NEW ListValidationMessage () .

        {Consultingwerk/foreachABL.i ValidationMessage oMessage in THIS-OBJECT}

            IF pcFieldName = oMessage:FieldName OR
               ((pcFieldName = ? OR pcFieldName = "":U) AND (oMessage:FieldName = ? OR oMessage:FieldName = "":U)) THEN

               oList:Add (oMessage) .
        END.

        RETURN oList .

    END METHOD.

    /**
     * Purpose: Parses an ERROR-STRING in the classic SmartComponent Library
     *          format and returns a ListValidationMessage instance
     * Notes:   Returns an empty list when the ERROR-STRING is empty
     * @param pcErrorString The ERROR-STRING to parse
     * @return The ListValidationMessage instance with the parsed message result
     */
    METHOD PUBLIC STATIC ListValidationMessage FromErrorString (pcErrorString AS CHARACTER):

        DEFINE VARIABLE oList    AS ListValidationMessage NO-UNDO .
        DEFINE VARIABLE oMessage AS ValidationMessage     NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER               NO-UNDO .
        DEFINE VARIABLE j        AS INTEGER               NO-UNDO .
        DEFINE VARIABLE iCount   AS INTEGER               NO-UNDO .
        DEFINE VARIABLE cEntry   AS CHARACTER             NO-UNDO .
        DEFINE VARIABLE cValues  AS CHARACTER EXTENT      NO-UNDO .

        oList = NEW ListValidationMessage () .

        iCount = NUM-ENTRIES (pcErrorString, CHR (3)) .

        entryLoop:
        DO i = 1 TO iCount:

            cEntry = ENTRY (i, pcErrorString, CHR (3)) .

            IF CharacterType:IsNullOrEmpty(cEntry)  THEN
                NEXT entryLoop .

            oMessage = NEW ValidationMessage () .
            oList:Add (oMessage) .

            IF NUM-ENTRIES (cEntry, CHR (4)) > 2 AND ENTRY (2, cEntry, CHR (4)) > "":U THEN
                ASSIGN oMessage:FieldName = ENTRY (2, cEntry, CHR (4)) .

            IF cEntry BEGINS "SmartMessage~t":U AND NUM-ENTRIES (cEntry, "~t":U) > 2 THEN DO:
                ASSIGN
                    cEntry                = ENTRY (1, cEntry, CHR (4))
                    oMessage:MessageGroup = ENTRY (2, cEntry, "~t":U)
                    oMessage:MessageId    = DataTypeHelper:ToInteger (ENTRY (3, cEntry, "~t":U)) .

                IF NUM-ENTRIES (cEntry, "~t":U) > 3 THEN DO:
                    EXTENT (cValues) = ?.
                    EXTENT (cValues) = NUM-ENTRIES (cEntry, "~t":U) - 3 .

                    DO j = 1 TO NUM-ENTRIES (cEntry, "~t":U) - 3:
                        cValues [j] = ENTRY (j + 3, cEntry, "~t":U) .
                    END.

                    oMessage:SubstitutionValues = cValues .
                END.
            END.

            ELSE
                oMessage:MessageStrings = ArrayHelper:Array (ENTRY (1, cEntry, CHR (4))) .

        END.

        RETURN oList .

    END METHOD.

    /**
     * Purpose: Returns a JSON Array with the members of the list
     * Notes:   Returns ? when the list is empty
     * @return The JsonArray instance with the members of the list
     */
    METHOD PUBLIC JsonArray ToJsonArray():

        DEFINE VARIABLE oJsonArray AS JsonArray NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO .

        IF THIS-OBJECT:Count = 0 THEN
            RETURN ? .

        iCount = THIS-OBJECT:Count .

        oJsonArray = NEW JsonArray (iCount) .

        DO i = 1 TO iCount:

            oJsonArray:Set(i, THIS-OBJECT:GetItem(i):ToJsonObject ()) .

        END.

        RETURN oJsonArray .

    END METHOD.

    /**
     * Purpose: Returns the string representation of the list
     * Notes:   Returns JSON String, when there is no JSON, returns ""
     * @return CHARACTER
     */
    METHOD PUBLIC OVERRIDE CHARACTER ToString():

        DEFINE VARIABLE oJsonArray AS JsonArray NO-UNDO .
        DEFINE VARIABLE lcJson     AS LONGCHAR  NO-UNDO .

        oJsonArray = THIS-OBJECT:ToJsonArray() .

        IF NOT VALID-OBJECT (oJsonArray) THEN
            RETURN "" .

        FIX-CODEPAGE (lcJson) = Codepages:UTF-8.
        oJsonArray:WRITE (INPUT-OUTPUT lcJson, TRUE) .

        RETURN STRING (lcJson) .

    END METHOD.

END CLASS.
