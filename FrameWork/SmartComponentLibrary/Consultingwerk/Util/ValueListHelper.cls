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
    File        : ValueListHelper
    Purpose     :
    Syntax      : Static methods only, Protected constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 01 16:49:22 CET 2010
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Assertion.*             FROM PROPATH .
USING Consultingwerk.Exceptions.*            FROM PROPATH .
USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Consultingwerk.Util.Enum.*             FROM PROPATH .
USING Consultingwerk.Windows.Framework.*     FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Infragistics.Win.*                     FROM ASSEMBLY .
&ENDIF
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Util.ValueListHelper:

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED ValueListHelper ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Builds a ValueList from a RetrieveData call to a Business Entity
        Notes:
        @param pcAppServerPartition The AppServer Partition to perform the request on, "" for default partition
        @param pcEntityName The Business Entity to retrieve data from
        @param pcEntityTable The name of the Business Entity Table to retrieve
        @param pcQueryString The Query String to retrieve data from
        @param pcKeyField The key field for the ValueList
        @param pcDisplayFields The fields to builds the DisplayText from
        @param pcDisplaySubstitute The SUBSTITUTE pattern to use for building the DisplayText, e.g. &1 (&2)
        @return The ValueList
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList FromBusinessEntity (pcAppServerPartition AS CHARACTER,
                                                       pcEntityName AS CHARACTER,
                                                       pcEntityTable AS CHARACTER,
                                                       pcQueryString AS CHARACTER,
                                                       pcKeyField AS CHARACTER,
                                                       pcDisplayFields AS CHARACTER,
                                                       pcDisplaySubstitute AS CHARACTER):

        RETURN FromBusinessEntity (pcAppServerPartition,
                                   pcEntityName,
                                   pcEntityTable,
                                   pcQueryString,
                                   pcKeyField,
                                   pcDisplayFields,
                                   pcDisplaySubstitute,
                                   "":U,
                                   "":U,
                                   ?,
                                   ?,
                                   ?) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a ValueList from a RetrieveData call to a Business Entity
        Notes:
        @param pcAppServerPartition The AppServer Partition to perform the request on, "" for default partition
        @param pcEntityName The Business Entity to retrieve data from
        @param pcEntityTable The name of the Business Entity Table to retrieve
        @param pcQueryString The Query String to retrieve data from
        @param pcKeyField The key field for the ValueList
        @param pcDisplayFields The fields to builds the DisplayText from
        @param pcDisplaySubstitute The SUBSTITUTE pattern to use for building the DisplayText, e.g. &1 (&2)
        @param pcFlagValue The value for the "Default" Entry
        @param pcFlagText The DisplayText for the "Default" Entry
        @return The ValueList
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList FromBusinessEntity (pcAppServerPartition AS CHARACTER,
                                                       pcEntityName AS CHARACTER,
                                                       pcEntityTable AS CHARACTER,
                                                       pcQueryString AS CHARACTER,
                                                       pcKeyField AS CHARACTER,
                                                       pcDisplayFields AS CHARACTER,
                                                       pcDisplaySubstitute AS CHARACTER,
                                                       pcFlagValue AS CHARACTER,
                                                       pcFlagText AS CHARACTER):

        RETURN FromBusinessEntity (pcAppServerPartition,
                                   pcEntityName,
                                   pcEntityTable,
                                   pcQueryString,
                                   pcKeyField,
                                   pcDisplayFields,
                                   pcDisplaySubstitute,
                                   pcFlagValue,
                                   pcFlagText,
                                   ?,
                                   ?,
                                   ?) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a ValueList from a RetrieveData call to a Business Entity
        Notes:
        @param pcAppServerPartition The AppServer Partition to perform the request on, "" for default partition
        @param pcEntityName The Business Entity to retrieve data from
        @param pcEntityTable The name of the Business Entity Table to retrieve
        @param pcQueryString The Query String to retrieve data from
        @param pcKeyField The key field for the ValueList
        @param pcDisplayFields The fields to builds the DisplayText from
        @param pcDisplaySubstitute The SUBSTITUTE pattern to use for building the DisplayText, e.g. &1 (&2)
        @param pcFlagValue The value for the "Default" Entry
        @param pcFlagText The DisplayText for the "Default" Entry
        @param pcImageField The name of the BLOB field to use to return an Image for the list entries
        @param pcImageKeyField The name of the CHARACTER field that contains the name of the field to build an image filename/key from
        @param pcImageKeyFieldSubstitute The SUBSTITUTE Expression for building the image filename/key with
        @return The ValueList
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList FromBusinessEntity (pcAppServerPartition AS CHARACTER,
                                                       pcEntityName AS CHARACTER,
                                                       pcEntityTable AS CHARACTER,
                                                       pcQueryString AS CHARACTER,
                                                       pcKeyField AS CHARACTER,
                                                       pcDisplayFields AS CHARACTER,
                                                       pcDisplaySubstitute AS CHARACTER,
                                                       pcFlagValue AS CHARACTER,
                                                       pcFlagText AS CHARACTER,
                                                       pcImageField AS CHARACTER,
                                                       pcImageKeyField AS CHARACTER,
                                                       pcImageKeyFieldSubstitute AS CHARACTER):

        DEFINE VARIABLE oFetchDataRequest AS IFetchDataRequest NO-UNDO .
        DEFINE VARIABLE hDataset          AS HANDLE            NO-UNDO .
        DEFINE VARIABLE hTable            AS HANDLE            NO-UNDO .

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcEntityName "'pcEntityName':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcEntityTable "'pcEntityTable':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcKeyField "'pcKeyField':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcDisplayFields "'pcDisplayFields':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcDisplaySubstitute "'pcDisplaySubstitute':U"} .

        oFetchDataRequest = NEW FetchDataRequest (pcEntityTable,
                                                  pcQueryString) .

        FrameworkSettings:ServiceAdapter:RetrieveData (pcAppServerPartition,
                                                       pcEntityName,
                                                       oFetchDataRequest,
                                                       OUTPUT DATASET-HANDLE hDataset) .

        ASSIGN hTable = hDataset:GET-BUFFER-HANDLE (pcEntityTable):TABLE-HANDLE .

        IF pcImageField > "":U THEN
            RETURN ValueListHelper:FromTempTable (hTable,
                                                  pcKeyField,
                                                  pcDisplayFields,
                                                  pcDisplaySubstitute,
                                                  pcFlagValue,
                                                  pcFlagText,
                                                  pcImageField) .
        ELSE
            IF pcImageKeyField > "":U AND
               pcImageKeyFieldSubstitute > "":U THEN

            RETURN ValueListHelper:FromTempTableAndImageProvider (hTable,
                                                                  pcKeyField,
                                                                  pcDisplayFields,
                                                                  pcDisplaySubstitute,
                                                                  pcImageKeyField,
                                                                  pcImageKeyFieldSubstitute,
                                                                  pcFlagValue,
                                                                  pcFlagText) .
        ELSE
            RETURN ValueListHelper:FromTempTable (hTable,
                                                  pcKeyField,
                                                  pcDisplayFields,
                                                  pcDisplaySubstitute,
                                                  pcFlagValue,
                                                  pcFlagText) .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a ValueList from a RetrieveData call to a Business Entity
        Notes:
        @param pcAppServerPartition The AppServer Partition to perform the request on, "" for default partition
        @param pcEntityName The Business Entity to retrieve data from
        @param pcEntityTable The name of the Business Entity Table to retrieve
        @param pcQueryString The Query String to retrieve data from
        @param pcKeyField The key field for the ValueList
        @param pcDisplayFields The fields to builds the DisplayText from
        @param pcDisplaySubstitute The SUBSTITUTE pattern to use for building the DisplayText, e.g. &1 (&2)
        @param pcQuerySort The sort expression for the query (or "")
        @return The ValueList
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList FromBusinessEntityWithSort (pcAppServerPartition AS CHARACTER,
                                                               pcEntityName AS CHARACTER,
                                                               pcEntityTable AS CHARACTER,
                                                               pcQueryString AS CHARACTER,
                                                               pcKeyField AS CHARACTER,
                                                               pcDisplayFields AS CHARACTER,
                                                               pcDisplaySubstitute AS CHARACTER,
                                                               pcQuerySort AS CHARACTER):

        RETURN FromBusinessEntityWithSort (pcAppServerPartition,
                                           pcEntityName,
                                           pcEntityTable,
                                           pcQueryString,
                                           pcKeyField,
                                           pcDisplayFields,
                                           pcDisplaySubstitute,
                                           "":U,
                                           "":U,
                                           ?,
                                           ?,
                                           ?,
                                           pcQuerySort) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a ValueList from a RetrieveData call to a Business Entity
        Notes:
        @param pcAppServerPartition The AppServer Partition to perform the request on, "" for default partition
        @param pcEntityName The Business Entity to retrieve data from
        @param pcEntityTable The name of the Business Entity Table to retrieve
        @param pcQueryString The Query String to retrieve data from
        @param pcKeyField The key field for the ValueList
        @param pcDisplayFields The fields to builds the DisplayText from
        @param pcDisplaySubstitute The SUBSTITUTE pattern to use for building the DisplayText, e.g. &1 (&2)
        @param pcFlagValue The value for the "Default" Entry
        @param pcFlagText The DisplayText for the "Default" Entry
        @param pcQuerySort The sort expression for the query (or "")
        @return The ValueList
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList FromBusinessEntityWithSort (pcAppServerPartition AS CHARACTER,
                                                               pcEntityName AS CHARACTER,
                                                               pcEntityTable AS CHARACTER,
                                                               pcQueryString AS CHARACTER,
                                                               pcKeyField AS CHARACTER,
                                                               pcDisplayFields AS CHARACTER,
                                                               pcDisplaySubstitute AS CHARACTER,
                                                               pcFlagValue AS CHARACTER,
                                                               pcFlagText AS CHARACTER,
                                                               pcQuerySort AS CHARACTER):

        RETURN FromBusinessEntityWithSort (pcAppServerPartition,
                                   pcEntityName,
                                   pcEntityTable,
                                   pcQueryString,
                                   pcKeyField,
                                   pcDisplayFields,
                                   pcDisplaySubstitute,
                                   pcFlagValue,
                                   pcFlagText,
                                   ?,
                                   ?,
                                   ?,
                                   pcQuerySort) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a ValueList from a RetrieveData call to a Business Entity
        Notes:
        @param pcAppServerPartition The AppServer Partition to perform the request on, "" for default partition
        @param pcEntityName The Business Entity to retrieve data from
        @param pcEntityTable The name of the Business Entity Table to retrieve
        @param pcQueryString The Query String to retrieve data from
        @param pcKeyField The key field for the ValueList
        @param pcDisplayFields The fields to builds the DisplayText from
        @param pcDisplaySubstitute The SUBSTITUTE pattern to use for building the DisplayText, e.g. &1 (&2)
        @param pcFlagValue The value for the "Default" Entry
        @param pcFlagText The DisplayText for the "Default" Entry
        @param pcImageField The name of the BLOB field to use to return an Image for the list entries
        @param pcImageKeyField The name of the CHARACTER field that contains the name of the field to build an image filename/key from
        @param pcImageKeyFieldSubstitute The SUBSTITUTE Expression for building the image filename/key with
        @param pcQueySort The sort expression for the query (or "")
        @return The ValueList
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList FromBusinessEntityWithSort (pcAppServerPartition AS CHARACTER,
                                                               pcEntityName AS CHARACTER,
                                                               pcEntityTable AS CHARACTER,
                                                               pcQueryString AS CHARACTER,
                                                               pcKeyField AS CHARACTER,
                                                               pcDisplayFields AS CHARACTER,
                                                               pcDisplaySubstitute AS CHARACTER,
                                                               pcFlagValue AS CHARACTER,
                                                               pcFlagText AS CHARACTER,
                                                               pcImageField AS CHARACTER,
                                                               pcImageKeyField AS CHARACTER,
                                                               pcImageKeyFieldSubstitute AS CHARACTER,
                                                               pcQueySort AS CHARACTER):

        DEFINE VARIABLE oFetchDataRequest AS IFetchDataRequest NO-UNDO .
        DEFINE VARIABLE hDataset          AS HANDLE            NO-UNDO .
        DEFINE VARIABLE hTable            AS HANDLE            NO-UNDO .

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcEntityName "'pcEntityName':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcEntityTable "'pcEntityTable':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcKeyField "'pcKeyField':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcDisplayFields "'pcDisplayFields':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcDisplaySubstitute "'pcDisplaySubstitute':U"} .

        oFetchDataRequest = NEW FetchDataRequest (pcEntityTable,
                                                  pcQueryString) .

        FrameworkSettings:ServiceAdapter:RetrieveData (pcAppServerPartition,
                                                       pcEntityName,
                                                       oFetchDataRequest,
                                                       OUTPUT DATASET-HANDLE hDataset) .

        ASSIGN hTable = hDataset:GET-BUFFER-HANDLE (pcEntityTable):TABLE-HANDLE .

        IF pcImageField > "":U THEN
            RETURN ValueListHelper:FromTempTableWithSort (hTable,
                                                          pcKeyField,
                                                          pcDisplayFields,
                                                          pcDisplaySubstitute,
                                                          pcFlagValue,
                                                          pcFlagText,
                                                          pcImageField,
                                                          pcQueySort) .
        ELSE
            IF pcImageKeyField > "":U AND
               pcImageKeyFieldSubstitute > "":U THEN

            RETURN ValueListHelper:FromTempTableAndImageProviderWithSort (hTable,
                                                                          pcKeyField,
                                                                          pcDisplayFields,
                                                                          pcDisplaySubstitute,
                                                                          pcImageKeyField,
                                                                          pcImageKeyFieldSubstitute,
                                                                          pcFlagValue,
                                                                          pcFlagText,
                                                                          pcQueySort) .
        ELSE
            RETURN ValueListHelper:FromTempTableWithSort (hTable,
                                                          pcKeyField,
                                                          pcDisplayFields,
                                                          pcDisplaySubstitute,
                                                          pcFlagValue,
                                                          pcFlagText,
                                                          pcQueySort) .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Turns an Enum into an Infragistics.Win.ValueList
        Notes:
        @param poClass the Progress.Lang.Class of the Enum
        @param poValueFrom The ValueFromEnum member that describes if the ValueList items will user the Enum's value (integer) or name as the data value
        @return The ValueList containing the values
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromEnum (poClass AS Progress.Lang.Class,
                                                              poValueFrom AS ValueFromEnum):

        DEFINE VARIABLE oList      AS CharacterList              NO-UNDO .
        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .
        DEFINE VARIABLE iValue     AS INTEGER                    NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER                    NO-UNDO .
        DEFINE VARIABLE cNames     AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE cValues    AS CHARACTER                  NO-UNDO .

        ASSIGN oValueList = NEW Infragistics.Win.ValueList () .

        ObjectAssert:IsValid (poClass, "poClass":U) .
        ObjectAssert:IsValid (poValueFrom, "poValueFrom":U) .

        IF poClass:IsA ("Consultingwerk.Enum":U) THEN DO:

            DO ON ERROR UNDO, THROW:
                /* Attempt to get names */
                oList = DYNAMIC-INVOKE (poClass:TypeName, "GetNames":U) .

                CATCH err AS Progress.Lang.Error:
                    UNDO, THROW NEW NotSupportedException(err, "GetNames":U, poClass:TypeName) .
                END CATCH.
            END.

            {Consultingwerk/foreachPrimitiveList.i Character cEntry IN oList}

                IF poValueFrom = ValueFromEnum:EnumName THEN
                    oValueList:ValueListItems:Add (BOX (cEntry), cEntry) .
                ELSE DO:
                    iValue = CAST (DYNAMIC-INVOKE (poClass:TypeName, "FromString":U, cEntry),
                                   Consultingwerk.Enum):Value .

                    oValueList:ValueListItems:Add (BOX (iValue), cEntry) .
                END.
            END.
        END.

        &IF DEFINED (NativeEnums) NE 0 &THEN
        ELSE IF poClass:IsA ("Progress.Lang.Enum":U) THEN DO:

            ASSIGN cNames  = poClass:GetEnumNames()
                   cValues = poClass:GetEnumValues() .

            DO i = 1 TO NUM-ENTRIES (cNames):
                ASSIGN cEntry = ENTRY (i, cNames) .

                IF poValueFrom = ValueFromEnum:EnumName THEN
                    oValueList:ValueListItems:Add (BOX (cEntry), cEntry) .
                ELSE DO:

                    ASSIGN iValue = INTEGER (ENTRY (i, cValues)) .

                    oValueList:ValueListItems:Add (BOX (iValue), cEntry) .
                END.
            END.
        END.
        &ENDIF

        ELSE
            UNDO, THROW NEW InvalidParameterValueException("poClass":U, poClass:TypeName, "Consultingwerk.Util.ValueListHelper":U) .

        RETURN oValueList .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns a comma-delimited list into an Infragistics.Win.ValueList
        Notes:
        @param pcList A comma delimited list with the Values for the ValueList
        @return The ValueList containing the values
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromList (pcList AS CHARACTER):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .
        DEFINE VARIABLE iEntries   AS INTEGER                    NO-UNDO.
        DEFINE VARIABLE i          AS INTEGER                    NO-UNDO.

        ASSIGN oValueList = NEW Infragistics.Win.ValueList ()
               iEntries   = NUM-ENTRIES (pcList).

        DO i = 1 TO iEntries:
            oValueList:ValueListItems:Add (ENTRY (i, pcList),
                                           ENTRY (i, pcList)) .
        END.

        RETURN oValueList .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns a list of Character primitives an Infragistics.Win.ValueList
        Notes:
        @param poList A CharacterList instance with the values for the ValueList
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromList (poList AS CharacterList):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .

        ASSIGN oValueList = NEW Infragistics.Win.ValueList () .

        {Consultingwerk/foreachPrimitiveList.i Character cItem in poList}
            oValueList:ValueListItems:Add (cItem) .
        END.

        RETURN oValueList .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns a list of NameValuePair's into an Infragistics.Win.ValueList
        Notes:   The Name portion of the NameValuePair's will be used as the DataValue and
                 the Value portion of the NameValuePair's will be used for the DisplayText
        @param poValueList A ListNameValuePair instance with the values for the ValueList
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromList (poValueList AS ListNameValuePair):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .

        ASSIGN oValueList = NEW Infragistics.Win.ValueList () .

        {Consultingwerk/foreachABL.i NameValuePair oItem in poValueList}
            oValueList:ValueListItems:Add (oItem:Name,
                                           oItem:Value) .
        END.

        RETURN oValueList .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns a comma-delimited list of list item pairs (similar to the ABL
                 LIST-ITEM-PAIRS property of the combo-box widget) into an
                 Infragistics.Win.ValueList
        Notes:
        @param pcList A comma delimited list with the Values for the ValueList
        @return The ValueList containing the values
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromListItemPairs (pcList AS CHARACTER):

        RETURN ValueListHelper:FromListItemPairs (pcList, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns a comma-delimited list of list item pairs (similar to the ABL
                 LIST-ITEM-PAIRS property of the combo-box widget) into an
                 Infragistics.Win.ValueList
        Notes:
        @param pcList A delimited list with the Values for the ValueList
        @param pcDelimiter The list delimiter
        @return The ValueList containing the values
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromListItemPairs (pcList AS CHARACTER,
                                                                       pcDelimiter AS CHARACTER):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .
        DEFINE VARIABLE iEntries   AS INTEGER                    NO-UNDO.
        DEFINE VARIABLE i          AS INTEGER                    NO-UNDO.

        ListAssert:EvenEntries (pcList, pcDelimiter) .

        ASSIGN oValueList = NEW Infragistics.Win.ValueList ()
               iEntries   = NUM-ENTRIES (pcList, pcDelimiter).

        DO i = 1 TO iEntries - 1 BY 2:
            oValueList:ValueListItems:Add (ENTRY (i + 1, pcList, pcDelimiter),
                                           ENTRY (i, pcList, pcDelimiter)) .
        END.

        RETURN oValueList .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields The name of the fieldto be used for the Display Value
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcFlagValue The CHARACTER value used for a Flag value (e.g. all or none)
        @param pcFlagText The Display Value for the flag value
        @param pcImageField The name of the BLOB field to use to return an Image for the list entries
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTable (phTable AS HANDLE,
                                                                   pcKeyField AS CHARACTER,
                                                                   pcDisplayFields AS CHARACTER,
                                                                   pcDisplaySubstitute AS CHARACTER,
                                                                   pcFlagValue AS CHARACTER,
                                                                   pcFlagText AS CHARACTER,
                                                                   pcImageField AS CHARACTER):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .

        oValueList = FromTempTable (phTable,
                                    pcKeyField,
                                    pcDisplayFields,
                                    pcDisplaySubstitute,
                                    pcImageField) .

        IF pcFlagText > "":U THEN
            oValueList:ValueListItems:Insert (0, pcFlagValue, pcFlagText) .

        RETURN oValueList .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields The name of the fieldto be used for the Display Value
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcFlagValue The CHARACTER value used for a Flag value (e.g. all or none)
        @param pcFlagText The Display Value for the flag value
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTable (phTable AS HANDLE,
                                                                   pcKeyField AS CHARACTER,
                                                                   pcDisplayFields AS CHARACTER,
                                                                   pcDisplaySubstitute AS CHARACTER,
                                                                   pcFlagValue AS CHARACTER,
                                                                   pcFlagText AS CHARACTER):

        RETURN ValueListHelper:FromTempTableWithSort (phTable,
                                                      pcKeyField,
                                                      pcDisplayFields,
                                                      pcDisplaySubstitute,
                                                      pcFlagValue,
                                                      pcFlagText,
                                                      "":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields The name of the fieldto be used for the Display Value
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcFlagValue The CHARACTER value used for a Flag value (e.g. all or none)
        @param pcFlagText The Display Value for the flag value
        @param pcQuerySort The sort expression for the query (or "")
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTableWithSort (phTable AS HANDLE,
                                                                           pcKeyField AS CHARACTER,
                                                                           pcDisplayFields AS CHARACTER,
                                                                           pcDisplaySubstitute AS CHARACTER,
                                                                           pcFlagValue AS CHARACTER,
                                                                           pcFlagText AS CHARACTER,
                                                                           pcQuerySort AS CHARACTER):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .

        oValueList = ValueListHelper:FromTempTableWithSort (phTable,
                                                            pcKeyField,
                                                            pcDisplayFields,
                                                            pcDisplaySubstitute,
                                                            pcQuerySort) .

        IF pcFlagText > "":U THEN
            oValueList:ValueListItems:Insert (0, pcFlagValue, pcFlagText) .

        RETURN oValueList .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields The name of the fieldto be used for the Display Value
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcFlagValue The CHARACTER value used for a Flag value (e.g. all or none)
        @param pcFlagText The Display Value for the flag value
        @param pcImageField The name of the BLOB field to use to return an Image for the list entries
        @param pcQuerySort The sort expression for the query (or "")
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTableWithSort (phTable AS HANDLE,
                                                                           pcKeyField AS CHARACTER,
                                                                           pcDisplayFields AS CHARACTER,
                                                                           pcDisplaySubstitute AS CHARACTER,
                                                                           pcFlagValue AS CHARACTER,
                                                                           pcFlagText AS CHARACTER,
                                                                           pcImageField AS CHARACTER,
                                                                           pcQuerySort AS CHARACTER):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .

        oValueList = FromTempTableWithSort (phTable,
                                            pcKeyField,
                                            pcDisplayFields,
                                            pcDisplaySubstitute,
                                            pcImageField,
                                            pcQuerySort) .

        IF pcFlagText > "":U THEN
            oValueList:ValueListItems:Insert (0, pcFlagValue, pcFlagText) .

        RETURN oValueList .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields The name of the fieldto be used for the Display Value
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param piFlagValue The INTEGER value used for a Flag value (e.g. all or none)
        @param pcFlagText The Display Value for the flag value
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTable (phTable AS HANDLE,
                                                                   pcKeyField AS CHARACTER,
                                                                   pcDisplayFields AS CHARACTER,
                                                                   pcDisplaySubstitute AS CHARACTER,
                                                                   piFlagValue AS INTEGER,
                                                                   pcFlagText AS CHARACTER):

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO .

        oValueList = FromTempTable (phTable,
                                    pcKeyField,
                                    pcDisplayFields,
                                    pcDisplaySubstitute,
                                    ?) .

        IF pcFlagText > "":U THEN
            oValueList:ValueListItems:Insert (0, piFlagValue, pcFlagText) .

        RETURN oValueList .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayField The name of the fieldto be used for the Display Value
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTable (phTable AS HANDLE,
                                                                   pcKeyField AS CHARACTER,
                                                                   pcDisplayField AS CHARACTER):

         RETURN FromTempTable (phTable,
                               pcKeyField,
                               pcDisplayField,
                               "&1":U,
                               ?) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields A comma-delimited list of fields to be used for the Display Value (SUBSTITUTE function used)
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTable (phTable AS HANDLE,
                                                                   pcKeyField AS CHARACTER,
                                                                   pcDisplayFields AS CHARACTER,
                                                                   pcDisplaySubstitute AS CHARACTER):

        RETURN ValueListHelper:FromTempTableWithSort (phTable,
                                                      pcKeyField,
                                                      pcDisplayFields,
                                                      pcDisplaySubstitute,
                                                      "") .

    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields A comma-delimited list of fields to be used for the Display Value (SUBSTITUTE function used)
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcQuerySort The sort expression for the query (or "")
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTableWithSort (phTable AS HANDLE,
                                                                           pcKeyField AS CHARACTER,
                                                                           pcDisplayFields AS CHARACTER,
                                                                           pcDisplaySubstitute AS CHARACTER,
                                                                           pcQuerySort AS CHARACTER):

         RETURN ValueListHelper:FromTempTableWithSort (phTable,
                                                       pcKeyField,
                                                       pcDisplayFields,
                                                       pcDisplaySubstitute,
                                                       ?,
                                                       pcQuerySort) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields A comma-delimited list of fields to be used for the Display Value (SUBSTITUTE function used)
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcImageField The name of the field that contains the image data to be used
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTable (phTable AS HANDLE,
                                                                   pcKeyField AS CHARACTER,
                                                                   pcDisplayFields AS CHARACTER,
                                                                   pcDisplaySubstitute AS CHARACTER,
                                                                   pcImageField AS CHARACTER):

        RETURN ValueListHelper:FromTempTableWithSort (phTable,
                                                      pcKeyField,
                                                      pcDisplayFields,
                                                      pcDisplaySubstitute,
                                                      pcImageField,
                                                      "") .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields A comma-delimited list of fields to be used for the Display Value (SUBSTITUTE function used)
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcImageField The name of the field that contains the image data to be used
        @param pcQuerySort The sort expression for the query (or "")
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.ValueList FromTempTableWithSort (phTable AS HANDLE,
                                                                           pcKeyField AS CHARACTER,
                                                                           pcDisplayFields AS CHARACTER,
                                                                           pcDisplaySubstitute AS CHARACTER,
                                                                           pcImageField AS CHARACTER,
                                                                           pcQuerySort AS CHARACTER):

        DEFINE VARIABLE hQuery         AS HANDLE                         NO-UNDO.

        DEFINE VARIABLE i              AS INTEGER                        NO-UNDO .
        DEFINE VARIABLE cValue         AS CHARACTER                      NO-UNDO EXTENT 5.
        DEFINE VARIABLE iEntries       AS INTEGER                        NO-UNDO .
        DEFINE VARIABLE mem_ptr        AS MEMPTR                         NO-UNDO .

        DEFINE VARIABLE oBytes         AS "System.Byte[]":U              NO-UNDO .
        DEFINE VARIABLE oStream        AS System.IO.MemoryStream         NO-UNDO .
        DEFINE VARIABLE oImage         AS System.Drawing.Bitmap          NO-UNDO .

        DEFINE VARIABLE oValueList     AS Infragistics.Win.ValueList     NO-UNDO .
        DEFINE VARIABLE oValueListItem AS Infragistics.Win.ValueListItem NO-UNDO .

        ASSIGN oValueList = NEW Infragistics.Win.ValueList ()
               iEntries   = NUM-ENTRIES (pcDisplayFields).

        IF pcQuerySort > "" THEN
            hQuery     = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (phTable:DEFAULT-BUFFER-HANDLE, SUBSTITUTE ("for each &1 &2":U,
                                                                                                                         phTable:NAME,
                                                                                                                         pcQuerySort)) .
        ELSE
            hQuery     = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (phTable:DEFAULT-BUFFER-HANDLE) .

        DO WHILE NOT hQuery:QUERY-OFF-END:

            DO i = 1 TO MINIMUM (5, iEntries):
                IF ENTRY (i, pcDisplayFields) > "":U THEN
                    cValue[i] = phTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (ENTRY (i, pcDisplayFields)):BUFFER-VALUE .
            END.

            oValueListItem = oValueList:ValueListItems:Add (BOX(phTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (pcKeyField):BUFFER-VALUE),
                                                            SUBSTITUTE (pcDisplaySubstitute,
                                                                        cValue[1], cValue[2], cValue[3], cValue[4], cValue[5])) .

            IF pcImageField > "":U THEN DO ON ERROR UNDO, THROW:
                oValueList:DisplayStyle = Infragistics.Win.ValueListDisplayStyle:DisplayTextAndPicture .

                COPY-LOB FROM phTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (pcImageField):BUFFER-VALUE
                         TO mem_ptr .

                ASSIGN oBytes = Consultingwerk.Util.DataTypeHelper:MemptrToByteArray (mem_ptr) .

                IF oBytes:Length > 0 THEN DO:
                    ASSIGN oStream = NEW System.IO.MemoryStream (oBytes)
                           oImage = NEW System.Drawing.Bitmap (oStream)

                           oValueListItem:Appearance:Image = oImage .

                    oStream:Close()  .
                    oStream:Dispose() .

                END.

                FINALLY:
                    SET-SIZE (mem_ptr) = 0 .
                END FINALLY.
            END.


            hQuery:GET-NEXT ().
        END.

        RETURN oValueList .

        FINALLY:
            IF VALID-HANDLE (hQuery) THEN
                DELETE OBJECT hQuery .
        END.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields A comma-delimited list of fields to be used for the Display Value (SUBSTITUTE function used)
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcImageKeyField The name of the field that contains the image key to be used
        @param pcFlagValue The CHARACTER value used for a Flag value (e.g. all or none)
        @param pcFlagText The Display Value for the flag value
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList FromTempTableAndImageProvider (phTable AS HANDLE,
                                                                  pcKeyField AS CHARACTER,
                                                                  pcDisplayFields AS CHARACTER,
                                                                  pcDisplaySubstitute AS CHARACTER,
                                                                  pcImageKeyField AS CHARACTER,
                                                                  pcFlagValue AS CHARACTER,
                                                                  pcFlagText AS CHARACTER):

        RETURN ValueListHelper:FromTempTableAndImageProvider (phTable,
                                                              pcKeyField,
                                                              pcDisplayFields,
                                                              pcDisplaySubstitute,
                                                              pcImageKeyField,
                                                              "&1":U,
                                                              pcFlagValue,
                                                              pcFlagText) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields A comma-delimited list of fields to be used for the Display Value (SUBSTITUTE function used)
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcImageKeyField The name of the field that contains the image key to be used
        @param pcImageKeyFieldSubstitute The SUBSTITUTE pattern to be used to build the actual image key to be used
        @param pcFlagValue The CHARACTER value used for a Flag value (e.g. all or none)
        @param pcFlagText The Display Value for the flag value
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList FromTempTableAndImageProvider (phTable AS HANDLE,
                                                                  pcKeyField AS CHARACTER,
                                                                  pcDisplayFields AS CHARACTER,
                                                                  pcDisplaySubstitute AS CHARACTER,
                                                                  pcImageKeyField AS CHARACTER,
                                                                  pcImageKeyFieldSubstitute AS CHARACTER,
                                                                  pcFlagValue AS CHARACTER,
                                                                  pcFlagText AS CHARACTER):

        RETURN ValueListHelper:FromTempTableAndImageProviderWithSort (phTable,
                                                                      pcKeyField,
                                                                      pcDisplayFields,
                                                                      pcDisplaySubstitute,
                                                                      pcImageKeyField,
                                                                      pcImageKeyFieldSubstitute,
                                                                      pcFlagValue,
                                                                      pcFlagText,
                                                                      "") .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Turns the records of a temp-table into an Infragistics.Win.ValueList
        Notes:
        @param phTable The HANDLE of the temp-table that contains the data
        @param pcKeyField The field of the temp-table to use for the Key value
        @param pcDisplayFields A comma-delimited list of fields to be used for the Display Value (SUBSTITUTE function used)
        @param pcDisplaySubstitute A base string in which the DisplayField values will be merged using the SUBSTITUTE function
        @param pcImageKeyField The name of the field that contains the image key to be used
        @param pcImageKeyFieldSubstitute The SUBSTITUTE pattern to be used to build the actual image key to be used
        @param pcFlagValue The CHARACTER value used for a Flag value (e.g. all or none)
        @param pcFlagText The Display Value for the flag value
        @param pcQuerySort The sort expression for the query (or "")
        @return The ValueList containing the values (key and display value)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList FromTempTableAndImageProviderWithSort (phTable AS HANDLE,
                                                                          pcKeyField AS CHARACTER,
                                                                          pcDisplayFields AS CHARACTER,
                                                                          pcDisplaySubstitute AS CHARACTER,
                                                                          pcImageKeyField AS CHARACTER,
                                                                          pcImageKeyFieldSubstitute AS CHARACTER,
                                                                          pcFlagValue AS CHARACTER,
                                                                          pcFlagText AS CHARACTER,
                                                                          pcQuerySort AS CHARACTER):

        DEFINE VARIABLE oImageProvider AS IImageProvider                 NO-UNDO .

        DEFINE VARIABLE hQuery         AS HANDLE                         NO-UNDO.

        DEFINE VARIABLE i              AS INTEGER                        NO-UNDO .
        DEFINE VARIABLE cValue         AS CHARACTER                      NO-UNDO EXTENT 5.
        DEFINE VARIABLE iEntries       AS INTEGER                        NO-UNDO .

        DEFINE VARIABLE oValueList     AS Infragistics.Win.ValueList     NO-UNDO .
        DEFINE VARIABLE oValueListItem AS Infragistics.Win.ValueListItem NO-UNDO .

        IF pcImageKeyField > "":U THEN DO:
            ASSIGN oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .

            {Consultingwerk/Assertion/ObjectAssert/IsValid.i oImageProvider """IImageProvider"":U"} .
        END.

        ASSIGN oValueList = NEW Infragistics.Win.ValueList ()
               iEntries   = NUM-ENTRIES (pcDisplayFields).

        IF pcQuerySort > "" THEN
            hQuery     = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (phTable:DEFAULT-BUFFER-HANDLE, SUBSTITUTE ("for each &1 &2":U,
                                                                                                                         phTable:NAME,
                                                                                                                         pcQuerySort)) .
        ELSE
            hQuery     = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (phTable:DEFAULT-BUFFER-HANDLE) .
        DO WHILE NOT hQuery:QUERY-OFF-END:

            DO i = 1 TO MINIMUM (5, iEntries):
                IF ENTRY (i, pcDisplayFields) > "":U THEN
                    cValue[i] = phTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (ENTRY (i, pcDisplayFields)):BUFFER-VALUE .
            END.

            oValueListItem = oValueList:ValueListItems:Add (BOX(phTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (pcKeyField):BUFFER-VALUE),
                                                            SUBSTITUTE (pcDisplaySubstitute,
                                                                        cValue[1], cValue[2], cValue[3], cValue[4], cValue[5])) .

            IF pcImageKeyField > "":U THEN DO ON ERROR UNDO, THROW:
                oValueList:DisplayStyle = Infragistics.Win.ValueListDisplayStyle:DisplayTextAndPicture .

                oValueListItem:Appearance:Image = oImageProvider:ImageForKey (SUBSTITUTE (pcImageKeyFieldSubstitute,
                                                                              phTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (pcImageKeyField):BUFFER-VALUE)) .
            END.

            hQuery:GET-NEXT ().
        END.

        IF pcFlagText > "":U THEN
            oValueList:ValueListItems:Insert (0, pcFlagValue, pcFlagText) .

        RETURN oValueList .

        FINALLY:
            IF VALID-HANDLE (hQuery) THEN
                DELETE OBJECT hQuery .
        END.

    END METHOD .

&ENDIF
END CLASS.
