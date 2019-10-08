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
    File        : BusinessEntityValidationGenerator
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike
    Created     : Tue May 26 20:54:32 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.*                                               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Validation.*             FROM PROPATH .
USING Progress.Lang.*                                                FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.BusinessEntityValidationGenerator IMPLEMENTS IBusinessEntityValidationGenerator:

    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY=REFERENCE-ONLY &ACCESS=PROTECTED}

    /*------------------------------------------------------------------------------
        Purpose: Generates the validation for the eField record currently in scope
        Notes:
        @param poValidation The validation properties instance
        @param pcTableName The name of the buffer to validate
        @param pcFieldName The name of the field to validate
        @param pcDataType The data type of the temp-table field
        @param piExtent The field extent
        @return The validation code block for the given field
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateFieldValidation (poValidation AS BaseValidationProperties,
                                                       pcTableName AS CHARACTER,
                                                       pcFieldName AS CHARACTER,
                                                       pcDataType AS CHARACTER,
                                                       piExtent AS INTEGER):

        DEFINE VARIABLE cCode AS LONGCHAR NO-UNDO.

        ASSIGN cCode = poValidation:GenerateCode(pcTableName, pcFieldName, pcDataType, piExtent) .

        IF cCode > "":U THEN
            RETURN SUBSTITUTE ("&1~n":U, StringHelper:Indent(cCode, 12)) .
        ELSE
            RETURN "":U .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Business Entity Validation Code
        Notes:
        @param dsBusinessEntity The Business Entity Dataset passed by-reference
        @return The generated validation code, including the @GeneratedValidationStart. and @GeneratedValidationEnd. Annotations
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateValidationCode (DATASET FOR dsBusinessEntity):

        DEFINE VARIABLE lFirst       AS LOGICAL                  NO-UNDO .
        DEFINE VARIABLE lcValidation AS LONGCHAR                 NO-UNDO .

        DEFINE VARIABLE lcCode       AS LONGCHAR                 NO-UNDO .
        DEFINE VARIABLE oInstance    AS BaseValidationProperties NO-UNDO .

        FIX-CODEPAGE (lcValidation) = Codepages:UTF-8 .

        ASSIGN lcCode = "        @GeneratedValidationStart.~n":U +
                        "        /* The following code is auto-generated. Don't insert custom code ~n":U +
                        "           between the @GeneratedValidationStart. and @GeneratedValidationEnd.~n":U +
                        "           annotations as it would get overwritten when you update the Business~n":U +
                        "           entity. */~n~n":U.

        FOR EACH eTable ON ERROR UNDO, THROW:
            ASSIGN lFirst = TRUE .

            FOR EACH eField WHERE eField.TempTableName = eTable.TempTableName ON ERROR UNDO, THROW:

                ASSIGN lcValidation = eField.FieldValidationInstance .

                IF lcValidation > "":U THEN DO:

                    IF lFirst THEN
                       ASSIGN lcCode = lcCode + SUBSTITUTE ("        FOR EACH &1 ON ERROR UNDO, THROW:~n":U,
                                                            eTable.TempTableName)
                              lFirst = FALSE .

                    oInstance = CAST (Serializable:DeserializeInstance (lcValidation),
                                      BaseValidationProperties) .

                    ASSIGN lcCode = lcCode + THIS-OBJECT:GenerateFieldValidation (oInstance, eField.TempTableName, eField.FieldName, eField.FieldDataType, eField.FieldExtent) .

                END.
            END.

            /* Was there at least a single validation instance for the table? */
            IF lFirst = FALSE THEN DO:
                ASSIGN lcCode = lcCode + "        END .~n~n":U .
            END.
        END .

        ASSIGN lcCode = TRIM (lcCode, "~n":U) + "~n        @GeneratedValidationEnd.":U .

        RETURN lcCode .

    END METHOD .

END CLASS.
