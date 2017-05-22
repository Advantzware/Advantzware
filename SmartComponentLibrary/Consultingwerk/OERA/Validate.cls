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
    File        : Validate
    Purpose     : Provides Assertion-Style validation routines for ProDataset
                  buffers
    Syntax      : Static methods
    Description : The methods in this class can be used in the Business Entity
                  or Data Access class validation hooks like this:
                  USING Consultingwerk.OERA.* FROM PROPATH .
                  Validate:IsNotNullOrEmpty (BUFFER eCustomer:HANDLE, "Name":U, "Customer Name is required to be filled.") .
                  The pcErrorMessage parameter is optionally - indicating that the
                  default message from the ValidationDefaultMessages class
                  will be returned
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 21 11:31:35 CEST 2013
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Validation+made+easy
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Assertion.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.* FROM PROPATH .
USING Consultingwerk.OERA.*           FROM PROPATH .
USING Consultingwerk.Util.*           FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

&SCOPED-DEFINE prefix       DEFINE VARIABLE hField AS HANDLE NO-UNDO.~
        BufferAssert:IsAvailable (phDatasetBuffer) .~
        BufferAssert:IsDatasetBuffer (phDatasetBuffer) .~
        BufferAssert:HasField (phDatasetBuffer, pcFieldName) .~
        ASSIGN hField = phDatasetBuffer:BUFFER-FIELD (pcFieldName) .

&SCOPED-DEFINE prefixrow       BufferAssert:IsAvailable (phDatasetBuffer) .~
        BufferAssert:IsDatasetBuffer (phDatasetBuffer) .

CLASS Consultingwerk.OERA.Validate:

    /**
     * Purpose: Static constructor for the Validate class
     * Notes:
     */
    CONSTRUCTOR STATIC Validate ():

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Validate class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED Validate ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Validates that a Business Entity record is accessible
     * Notes:   &1 can be passed as part of the query string and will be replaced
     *          (substitute) with the current field value of the source buffer
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate (the field's value may be used for &1 in the pcErrorMessage)
     * @param pcBusinessEntityName The name of the Business Entity
     * @param pcTempTableName The name of the temp-table
     * @param pcQueryString The query string, e.g. "FOR EACH eSalesRep WHERE eSalesrep.SalesRep = 'BBB'" OR "WHERE eSalesrep.SalesRep = 'BBB'" OR "eSalesrep.SalesRep = 'BBB'"
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the record can be found
     */
    METHOD PUBLIC STATIC LOGICAL CanFind (phDatasetBuffer AS HANDLE,
                                          pcFieldName AS CHARACTER,
                                          pcBusinessEntityName AS CHARACTER,
                                          pcTempTableName AS CHARACTER,
                                          pcQueryString AS CHARACTER,
                                          pcErrorMessage AS CHARACTER):

        DEFINE VARIABLE oParameter  AS CanFindParameter NO-UNDO .
        DEFINE VARIABLE hDataset    AS HANDLE           NO-UNDO .
        DEFINE VARIABLE cFindString AS CHARACTER        NO-UNDO .

        {&prefix}

        IF pcQueryString BEGINS "FOR EACH ":U THEN DO:
            ASSIGN cFindString = SUBSTRING (pcQueryString, 10, -1, "CHARACTER":U) .

            ENTRY (1, cFindString, " ":U) = "":U .

            cFindString = TRIM (cFindString) .
        END.
        ELSE
            ASSIGN cFindString = pcQueryString .

        oParameter = NEW CanFindParameter (SUBSTITUTE (cFindString,
                                                       QUOTER (phDatasetBuffer:BUFFER-FIELD (pcFieldName):BUFFER-VALUE)),
                                           pcTempTableName) .

        ServiceInterface:InvokeMethod (pcBusinessEntityName,
                                       "CanFind":U,
                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                       oParameter) .

        IF oParameter:Result = FALSE THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i CanFind 101 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Validates that a character field is Empty
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsEmpty (phDatasetBuffer AS HANDLE,
                                          pcFieldName AS CHARACTER,
                                          pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF hField:BUFFER-VALUE = "":U THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsEmpty 105 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field value is in a given list
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcList The list of valid values
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsInList (phDatasetBuffer AS HANDLE,
                                           pcFieldName AS CHARACTER,
                                           pcList AS CHARACTER,
                                           pcErrorMessage AS CHARACTER):

        RETURN IsInList (phDatasetBuffer,
                         pcFieldName,
                         pcList,
                         ",":U,
                         pcErrorMessage) .

    END METHOD .

    /**
     * Purpose: Validates that a character field value is in a given list
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcList The list of valid values
     * @param pcDelimeter The list delimeter character
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsInList (phDatasetBuffer AS HANDLE,
                                           pcFieldName AS CHARACTER,
                                           pcList AS CHARACTER,
                                           pcDelimeter AS CHARACTER,
                                           pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF LOOKUP (hField:BUFFER-VALUE, pcList, pcDelimeter) > 0 THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsInList 106 "BufferHelper:BufferFieldValue(hField), pcList, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that an integer field is in a given range
     * Notes:   Upper limit and lower limit are considered valid values
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param piLowerLimit The lower limit of the range
     * @param piUpperLimit The upper limit of the range
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsInRange (phDatasetBuffer AS HANDLE,
                                            pcFieldName AS CHARACTER,
                                            piLowerLimit AS INTEGER,
                                            piUpperLimit AS INTEGER,
                                            pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE >= piLowerLimit AND hField:BUFFER-VALUE <= piUpperLimit THEN
            RETURN TRUE .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsInRange 107 "BufferHelper:BufferFieldValue(hField), STRING(piLowerLimit), hField:LABEL, STRING(piUpperLimit)"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

    END METHOD .

&IF DEFINED (SmartFramework) NE 0 OR DEFINED (SmartFrameworkBackend) NE 0 &THEN
    /**
     * Purpose: Validates that the given data item is not restricted by the security
     *          services
     * Notes:   Provides data security based on the SmartFramework ISecurityService
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcSecurityRealmCode The code of the security realm to validate the field against
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field's value is not restricted
     */
    METHOD PUBLIC STATIC LOGICAL IsNotRestricted (phDatasetBuffer AS HANDLE,
                                                  pcFieldName AS CHARACTER,
                                                  pcSecurityRealmCode AS CHARACTER,
                                                  pcErrorMessage AS CHARACTER):

        DEFINE VARIABLE oSecurityService AS Consultingwerk.SmartFramework.Authorization.ISecurityService NO-UNDO .

        {&prefix}

        oSecurityService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ISecurityService} .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i  oSecurityService """Consultingwerk.SmartFramework.Authorization.ISecurityService"":U"} .

        IF oSecurityService:IsRestricted (pcSecurityRealmCode, hField:BUFFER-VALUE) THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsNotRestricted 109 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD.
&ENDIF

    /**
     * Purpose: Validates that a field's value is not ?
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsNotUnknown (phDatasetBuffer AS HANDLE,
                                               pcFieldName AS CHARACTER,
                                               pcErrorMessage AS CHARACTER):

        {&prefix}

        IF hField:BUFFER-VALUE = ? THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsNotUnknown 110 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field's value is not unknown value or zero
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsNotUnknownOrZero (phDatasetBuffer AS HANDLE,
                                                     pcFieldName AS CHARACTER,
                                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE = 0 OR hField:BUFFER-VALUE = ? THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsNotUnknownOrZero 111 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

&IF DEFINED (SmartFramework) NE 0 OR DEFINED (SmartFrameworkBackend) NE 0 &THEN
    /**
     * Purpose: Validates that the user has authorization for the given security
     *          token. This is not a field level validation. This is a row level
     *          validation
     * Notes:   Provides data security based on the SmartFramework ITokenSecurityService
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcSecurityTokenCode The code of the security token to validate the
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field's value is not restricted
     */
    METHOD PUBLIC STATIC LOGICAL IsTokenNotRestricted (phDatasetBuffer AS HANDLE,
                                                       pcSecurityTokenCode AS CHARACTER,
                                                       pcErrorMessage AS CHARACTER):

        DEFINE VARIABLE oTokenSecurityService AS Consultingwerk.SmartFramework.Authorization.ITokenSecurityService NO-UNDO .

        {&prefixrow}

        oTokenSecurityService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ITokenSecurityService} .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i  oTokenSecurityService """Consultingwerk.SmartFramework.Authorization.ITokenSecurityService"":U"} .

        IF oTokenSecurityService:IsTokenRestricted (pcSecurityTokenCode) THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsTokenNotRestricted 112 "?, pcSecurityTokenCode, ?"}

            DatasetHelper:AddErrorString (phDatasetBuffer, pcErrorMessage) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD.
&ENDIF

    /**
     * Purpose: Validates that a field's value is ?
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsUnknown (phDatasetBuffer AS HANDLE,
                                            pcFieldName AS CHARACTER,
                                            pcErrorMessage AS CHARACTER):

        {&prefix}

        IF hField:BUFFER-VALUE <> ? THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsUnknown 113 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field's value is unknown or zero
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsUnknownOrZero (phDatasetBuffer AS HANDLE,
                                                  pcFieldName AS CHARACTER,
                                                  pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE = 0 OR hField:BUFFER-VALUE = ? THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsUnknownOrZero 114 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field's value is a valid email address
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsValidEmailAddress (phDatasetBuffer AS HANDLE,
                                                      pcFieldName AS CHARACTER,
                                                      pcErrorMessage AS CHARACTER):

        RETURN IsValidEmailAddress (phDatasetBuffer, pcFieldName, FALSE, pcErrorMessage).

    END METHOD.

    /**
     * Purpose: Validates that a character field's value is a valid email address
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param plValidateMX Optionaly validate the MX of the given Domain
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsValidEmailAddress (phDatasetBuffer AS HANDLE,
                                                      pcFieldName AS CHARACTER,
                                                      plValidateMX AS LOGICAL,
                                                      pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF NetworkHelper:ValidateEmailAddress (hField:BUFFER-VALUE, plValidateMX) THEN .
        ELSE DO:
            {Consultingwerk/OERA/validate-default-message.i IsInvalidEmailAddress 125 "BufferHelper:BufferFieldValue (hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD.

    /**
     * Purpose: Validates that a date field's value is on the given day of the week
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param poWeekDay The WeekDayEnum value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsWeekday (phDatasetBuffer AS HANDLE,
                                            pcFieldName AS CHARACTER,
                                            poWeekDay AS WeekDayEnum,
                                            pcErrorMessage AS CHARACTER):

        {&prefix}

        IF Consultingwerk.OERA.Validate:IsUnknown (phDatasetBuffer, pcFieldName, pcErrorMessage) = FALSE THEN
            RETURN FALSE .

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATE + ",":U +
                                                                  DataTypeEnum:DATETIME   + ",":U +
                                                                  DataTypeEnum:DATETIME-TZ) .

        IF WeekDayEnum:FromAblWeekDay (WEEKDAY (hField:BUFFER-VALUE)) = poWeekday THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsSpecificWeekday 116 "BufferHelper:BufferFieldValue(hField), poWeekDay:ToString(), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a date field's value is a weekday (Monday - Friday)
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsWeekday (phDatasetBuffer AS HANDLE,
                                            pcFieldName AS CHARACTER,
                                            pcErrorMessage AS CHARACTER):

        DEFINE VARIABLE iWeekday AS INTEGER NO-UNDO.

        {&prefix}

        IF Consultingwerk.OERA.Validate:IsUnknown (phDatasetBuffer, pcFieldName, pcErrorMessage) = FALSE THEN
            RETURN FALSE .

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATE + ",":U +
                                                                  DataTypeEnum:DATETIME   + ",":U +
                                                                  DataTypeEnum:DATETIME-TZ) .

        ASSIGN iWeekday = WEEKDAY (hField:BUFFER-VALUE) .

        IF iWeekday = 1 OR iWeekday = 7 THEN DO:
            /* SCL-1425 - support for default validation messages */
            IF pcErrorMessage = "":U OR pcErrorMessage = ? THEN
                ASSIGN pcErrorMessage = SUBSTITUTE (ValidationDefaultMessages:IsWeekday, hField:BUFFER-VALUE, ?, hField:LABEL) .
            {Consultingwerk/OERA/validate-default-message.i IsWeekday 115 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field's value is zero
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsZero (phDatasetBuffer AS HANDLE,
                                         pcFieldName AS CHARACTER,
                                         pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE = 0 THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsZero 117 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field is equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL EQ (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pcValue AS CHARACTER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF hField:BUFFER-VALUE = pcValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i EQ 102 "BufferHelper:BufferFieldValue(hField), pcValue, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pcValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a date field is is equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL EQ (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATE,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATE) .

        IF hField:BUFFER-VALUE = pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i EQ 102 "BufferHelper:BufferFieldValue(hField), STRING (pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime field is is equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL EQ (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME) .

        IF hField:BUFFER-VALUE = pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i EQ 102 "BufferHelper:BufferFieldValue(hField), STRING (pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime-tz field is is equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL EQ (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME-TZ,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME-TZ) .

        IF hField:BUFFER-VALUE = pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i EQ 102 "BufferHelper:BufferFieldValue(hField), STRING (pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param piValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL EQ (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     piValue AS INTEGER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE = piValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i EQ 102 "BufferHelper:BufferFieldValue(hField), STRING (piValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, piValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdeValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL EQ (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdeValue AS DECIMAL,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE = pdeValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i EQ 102 "BufferHelper:BufferFieldValue(hField), STRING (pdeValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdeValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field is greater than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pcValue AS CHARACTER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF hField:BUFFER-VALUE >= pcValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GE 103 "BufferHelper:BufferFieldValue(hField), pcValue, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pcValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a date field is greater than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATE,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATE) .


        IF hField:BUFFER-VALUE >= pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GE 103 "BufferHelper:BufferFieldValue(hField), STRING (pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime field is greater than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME) .


        IF hField:BUFFER-VALUE >= pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GE 103 "BufferHelper:BufferFieldValue(hField), STRING (pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime-tz field is greater than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME-TZ,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME-TZ) .


        IF hField:BUFFER-VALUE >= pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GE 103 "BufferHelper:BufferFieldValue(hField), STRING (pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is greater than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param piValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     piValue AS INTEGER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE >= piValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GE 103 "BufferHelper:BufferFieldValue(hField), STRING (piValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, piValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is greater than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdeValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdeValue AS DECIMAL,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE >= pdeValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GE 103 "BufferHelper:BufferFieldValue(hField), STRING (pdeValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdeValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field is greater than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pcValue AS CHARACTER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF hField:BUFFER-VALUE > pcValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GT 104 "BufferHelper:BufferFieldValue(hField), pcValue, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pcValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a date field is greater than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATE,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATE) .

        IF hField:BUFFER-VALUE > pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GT 104 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime field is greater than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME) .

        IF hField:BUFFER-VALUE > pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GT 104 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime-tz field is greater than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME-TZ,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME-TZ) .

        IF hField:BUFFER-VALUE > pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GT 104 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is greater than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param piValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     piValue AS INTEGER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE > piValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GT 104 "BufferHelper:BufferFieldValue(hField), STRING(piValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, piValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is greater than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdeValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL GT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdeValue AS DECIMAL,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE > pdeValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i GT 104 "BufferHelper:BufferFieldValue(hField), STRING(pdeValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdeValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field is not Null or Empty
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL IsNotNullOrEmpty (phDatasetBuffer AS HANDLE,
                                                   pcFieldName AS CHARACTER,
                                                   pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF CharacterType:IsNullOrEmpty (hField:BUFFER-VALUE) THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i IsNotNullOrEmpty 108 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field is less than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pcValue AS CHARACTER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF hField:BUFFER-VALUE <= pcValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LE 118 "BufferHelper:BufferFieldValue(hField), pcValue, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pcValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a date field is less than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATE,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATE) .

        IF hField:BUFFER-VALUE <= pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LE 118 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime field is less than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME) .

        IF hField:BUFFER-VALUE <= pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LE 118 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime-tz field is less than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME-TZ,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME-TZ) .

        IF hField:BUFFER-VALUE <= pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LE 118 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is less than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdeValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdeValue AS DECIMAL,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE <= pdeValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LE 118 "BufferHelper:BufferFieldValue(hField), STRING(pdeValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdeValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is less than or equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param piValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     piValue AS INTEGER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE <= piValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LE 118 "BufferHelper:BufferFieldValue(hField), STRING(piValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, piValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field is less than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pcValue AS CHARACTER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF hField:BUFFER-VALUE < pcValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LT 119 "BufferHelper:BufferFieldValue(hField), pcValue, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pcValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a date field is less than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATE,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATE) .

        IF hField:BUFFER-VALUE < pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LT 119 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime field is less than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME) .

        IF hField:BUFFER-VALUE < pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LT 119 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime-tz field is less than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME-TZ,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME-TZ) .

        IF hField:BUFFER-VALUE < pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LT 119 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is less than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdeValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdeValue AS DECIMAL,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE < pdeValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LT 119 "BufferHelper:BufferFieldValue(hField), STRING(pdeValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdeValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is less than a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param piValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL LT (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     piValue AS INTEGER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE < piValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i LT 119 "BufferHelper:BufferFieldValue(hField), STRING(piValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, piValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field's value is not longer than the given
     *          length
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param piMaxLength The maximum length for the character field
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 as a place holder for the length
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL MaxLength (phDatasetBuffer AS HANDLE,
                                            pcFieldName AS CHARACTER,
                                            piMaxLength AS INTEGER,
                                            pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF LENGTH (hField:BUFFER-VALUE, "CHARACTER":U) > piMaxLength THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i MaxLength 120 "BufferHelper:BufferFieldValue(hField), STRING(piMaxLength), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, piMaxLength), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field's value is not shorter than the given
     *          length
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param piMinLength The maximum length for the character field
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 as a place holder for the minimum length
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL MinLength (phDatasetBuffer AS HANDLE,
                                            pcFieldName AS CHARACTER,
                                            piMinLength AS INTEGER,
                                            pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF LENGTH (hField:BUFFER-VALUE, "CHARACTER":U) < piMinLength THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i MinLength 121 "BufferHelper:BufferFieldValue(hField), STRING(piMinLength), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, piMinLength), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a character field is not equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL NE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pcValue AS CHARACTER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:CHARACTER) .

        IF hField:BUFFER-VALUE <> pcValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i NE 122 "BufferHelper:BufferFieldValue(hField), pcValue, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pcValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a date field is is not equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL NE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATE,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATE) .

        IF hField:BUFFER-VALUE <> pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i NE 122 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime field is is not equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL NE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME) .

        IF hField:BUFFER-VALUE <> pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i NE 122 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a datetime-tz field is is not equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdtValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL NE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdtValue AS DATETIME-TZ,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DATETIME-TZ) .

        IF hField:BUFFER-VALUE <> pdtValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i NE 122 "BufferHelper:BufferFieldValue(hField), STRING(pdtValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdtValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is not equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param piValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL NE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     piValue AS INTEGER,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE <> piValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i NE 122 "BufferHelper:BufferFieldValue(hField), STRING(piValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, piValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a numeric field is not equal to a value
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pdeValue The value to compare
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value and &2 for the compared value
     * @return True when the field meets the criteria
     */
    METHOD PUBLIC STATIC LOGICAL NE (phDatasetBuffer AS HANDLE,
                                     pcFieldName AS CHARACTER,
                                     pdeValue AS DECIMAL,
                                     pcErrorMessage AS CHARACTER):

        {&prefix}

        BufferAssert:FieldDataType (phDatasetBuffer, pcFieldName, DataTypeEnum:DECIMAL + ",":U +
                                                                  DataTypeEnum:INT64   + ",":U +
                                                                  DataTypeEnum:INTEGER) .

        IF hField:BUFFER-VALUE <> pdeValue THEN .
        ELSE DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i NE 122 "BufferHelper:BufferFieldValue(hField), STRING(pdeValue), hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE, pdeValue), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Validates that a Business Entity record is not available
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate (the field's value may be used for &1 in the pcErrorMessage)
     * @param pcBusinessEntityName The name of the Business Entity
     * @param pcTempTableName The name of the temp-table
     * @param pcQueryString The query string, e.g. "FOR EACH eSalesRep WHERE eSalesrep.SalesRep = 'BBB'"
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the field value
     * @return True when the record can be found
     */
    METHOD PUBLIC STATIC LOGICAL NotCanFind (phDatasetBuffer AS HANDLE,
                                             pcFieldName AS CHARACTER,
                                             pcBusinessEntityName AS CHARACTER,
                                             pcTempTableName AS CHARACTER,
                                             pcQueryString AS CHARACTER,
                                             pcErrorMessage AS CHARACTER):

        DEFINE VARIABLE oFetchDataRequest AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE hDataset          AS HANDLE           NO-UNDO .

        {&prefix}

        ASSIGN pcQueryString = SUBSTITUTE (pcQueryString,
                                           QUOTER (phDatasetBuffer:BUFFER-FIELD (pcFieldName):BUFFER-VALUE))

               oFetchDataRequest = NEW FetchDataRequest (pcTempTableName,
                                                         pcQueryString,
                                                         1) .

        ServiceInterface:FetchData (pcBusinessEntityName,
                                    oFetchDataRequest,
                                    OUTPUT DATASET-HANDLE hDataset) .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hDataset """Dataset"":U"} .

        DatasetAssert:HasBuffer (hDataset,
                                 pcTempTableName) .

        hDataset:GET-BUFFER-HANDLE (pcTempTableName):FIND-FIRST () NO-ERROR .

        IF hDataset:GET-BUFFER-HANDLE (pcTempTableName):AVAILABLE THEN DO:
            /* SCL-1425 - support for default validation messages */
            {Consultingwerk/OERA/validate-default-message.i NotCanFind 123 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

            DatasetHelper:AddErrorString (phDatasetBuffer, SUBSTITUTE (pcErrorMessage, hField:BUFFER-VALUE), pcFieldName, phDatasetBuffer:NAME) .
            RETURN FALSE .
        END.

        RETURN TRUE .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .

            ErrorHelper:ResetErrorStatus() .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Validates that a field is not changed
     * Notes:
     * @param phDatasetBuffer The handle to the ProDataset Buffer
     * @param pcFieldName The field name to validate
     * @param pcErrorMessage The error message, optionally using &1 as a place holder for the current field value and &2 for previous field value (BEFORE-TABLE)
     * @return True when the field was not changed or the row-state is not row-modified
     */
    METHOD PUBLIC STATIC LOGICAL ReadOnly (phDatasetBuffer AS HANDLE,
                                           pcFieldName AS CHARACTER,
                                           pcErrorMessage AS CHARACTER):

        {&prefix}

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF phDatasetBuffer:ROW-STATE <> ROW-MODIFIED THEN
            RETURN TRUE .

        BufferHelper:FindBeforeBuffer (phDatasetBuffer) .

        IF hField:EXTENT > 1 THEN
        DO:
            DO i = 1 TO hField:EXTENT:
                IF hField:BUFFER-VALUE(i) <> phDatasetBuffer:BEFORE-BUFFER:BUFFER-FIELD (pcFieldName):BUFFER-VALUE(i) THEN DO:
                    /* SCL-1425 - support for default validation messages */
                    {Consultingwerk/OERA/validate-default-message.i ReadOnly 124 "hField:BUFFER-VALUE(i), ?, hField:LABEL"}

                    DatasetHelper:AddErrorString (phDatasetBuffer,
                                                  SUBSTITUTE (pcErrorMessage,
                                                              hField:BUFFER-VALUE(i),
                                                              phDatasetBuffer:BEFORE-BUFFER:BUFFER-FIELD (pcFieldName):BUFFER-VALUE(i)),
                                                  pcFieldName,
                                                  phDatasetBuffer:NAME) .
                    RETURN FALSE .
                END.
            END.
        END .
        ELSE
            IF hField:BUFFER-VALUE <> phDatasetBuffer:BEFORE-BUFFER:BUFFER-FIELD (pcFieldName):BUFFER-VALUE THEN DO:
                /* SCL-1425 - support for default validation messages */
                {Consultingwerk/OERA/validate-default-message.i ReadOnly 124 "BufferHelper:BufferFieldValue(hField), ?, hField:LABEL"}

                DatasetHelper:AddErrorString (phDatasetBuffer,
                                              SUBSTITUTE (pcErrorMessage,
                                                          hField:BUFFER-VALUE,
                                                          phDatasetBuffer:BEFORE-BUFFER:BUFFER-FIELD (pcFieldName):BUFFER-VALUE),
                                              pcFieldName,
                                              phDatasetBuffer:NAME) .
                RETURN FALSE .
            END.

        RETURN TRUE .

    END METHOD.

END CLASS.
