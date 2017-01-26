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
    File        : TempTableFactory
    Purpose     : Reflects into a .NET System.Type and creates a dynamic
                  temp-table for selected properties of the System.Type
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 18 08:59:53 CET 2014
    Notes       : SCL-591
                  http://jira.consultingwerkcloud.com:8090/display/SCL/.NET+Value+object+to+ABL+Temp-Table+mapping
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                      FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Windows.TempTableObjectMapping.* FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Reflection.*                             FROM ASSEMBLY .
USING Progress.Util.*                                 FROM ASSEMBLY.
&ENDIF
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.TempTableObjectMapping.TempTableFromTypeFactory
    IMPLEMENTS ITempTableFromTypeFactory:

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Returns the ABL DataType that should be used to store the values of
                 the property
        Notes:   Assumes CHARACTER for System.String, not LONGCHAR or BLOB
        @param poType The type of the property
        @return The ABL DataType to use
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER AblDataType (poType AS System.Type):

        IF poType:IsEnum THEN
            RETURN "INTEGER":U .

        RETURN DataTypeHelper:ToAblDataType (poType) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Builds an ABL temp-table from a .NET System.Type
       Notes:
       @param poType The reference to the System.Type to build the temp-table from
       @param pcProperties The comma delimited list of property specs, supported entries are PropertyName or PropertyName|FieldName or PropertyName|FieldName(ABL DataType)
       @param pcUniqueKeyProperties The comma delimited list of property names that build the PUK, must be form the list in pcProperties
       @param pcTempTableName The name for the temp-table to create
       @return The handle of the created temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE CreateTempTable (poType AS System.Type,
                                          pcProperties AS CHARACTER,
                                          pcUniqueKeyProperties AS CHARACTER,
                                          pcTempTableName AS CHARACTER):

        DEFINE VARIABLE hTempTable       AS HANDLE       NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER      NO-UNDO .
        DEFINE VARIABLE iEntries         AS INTEGER      NO-UNDO .

        DEFINE VARIABLE cEntry           AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE iIndex           AS INTEGER      NO-UNDO .
        DEFINE VARIABLE cPropertyName    AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE cFieldName       AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE cDataType        AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE oProperty        AS PropertyInfo NO-UNDO .
        DEFINE VARIABLE iBracket1        AS INTEGER      NO-UNDO .
        DEFINE VARIABLE iBracket2        AS INTEGER      NO-UNDO .

        ObjectAssert:IsValid (poType, "poType":U) .
        Assert:NotNullOrEmpty (pcProperties, "pcProperties":U) .
        Assert:NotNullOrEmpty (pcUniqueKeyProperties, "pcUniqueKeyProperties":U) .
        Assert:NotNullOrEmpty (pcTempTableName, "pcTempTableName":U) .

        CREATE TEMP-TABLE hTempTable .

        hTempTable:ADD-NEW-INDEX (SUBSTITUTE ("&1Primary":U, pcTempTableName), TRUE, TRUE, FALSE) .

        ASSIGN iEntries = NUM-ENTRIES (pcProperties) .

        DO i = 1 TO iEntries:

            ASSIGN cEntry        = ENTRY (i, pcProperties)
                   cPropertyName = ENTRY (1, cEntry, "|":U) .

            oProperty = poType:GetProperty (cPropertyName) .

            IF NOT VALID-OBJECT (oProperty) THEN
                UNDO, THROW NEW InvalidPropertyNameException (SUBSTITUTE ("The property name &1 is not available in &2"{&TRAN},
                                                                          cPropertyName, poType:FullName),
                                                              cPropertyName,
                                                              poType:FullName) .

            IF NUM-ENTRIES (cEntry, "|":U) > 1 THEN DO:

                ASSIGN cEntry    = ENTRY (2, cEntry, "|":U)
                       iIndex    = INDEX (cEntry, ".":U) .

                IF iIndex > 0 THEN
                    ASSIGN cEntry = SUBSTRING (cEntry, iIndex + 1) .

                ASSIGN iBracket1 = INDEX (cEntry, "(":U).

                IF iBracket1 = 0 THEN
                    ASSIGN cFieldName = cEntry
                           cDataType  = THIS-OBJECT:AblDataType (oProperty:PropertyType) .
                ELSE DO:
                    ASSIGN cFieldName = SUBSTRING (cEntry, 1, iBracket1 - 1)
                           iBracket2  = StringHelper:FindClosingBracket (cEntry, iBracket1)
                           cDataType  = SUBSTRING (cEntry, iBracket1 + 1, iBracket2 - iBracket1 - 1).
                END.
            END.
            ELSE
                ASSIGN cFieldName = cPropertyName
                       cDataType  = THIS-OBJECT:AblDataType (oProperty:PropertyType) .

            hTempTable:ADD-NEW-FIELD (cFieldName, cDataType) .

            IF LOOKUP (cPropertyName, pcUniqueKeyProperties) > 0 THEN
                hTempTable:ADD-INDEX-FIELD (SUBSTITUTE ("&1Primary":U, pcTempTableName), cFieldName) .
        END.

        hTempTable:TEMP-TABLE-PREPARE (pcTempTableName) .

        RETURN hTempTable.

        /* Clean up in case of error */
        CATCH err AS Progress.Lang.Error:

            IF VALID-HANDLE (hTempTable) THEN
               DELETE OBJECT hTempTable .

            ASSIGN hTempTable = ? .

            UNDO, THROW err.
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Builds an ABL temp-table from a .NET System.Type
       Notes:   Defaults to using the short type name as the temp-table name
       @param poType The reference to the System.Type to build the temp-table from
       @param pcProperties The comma delimited list of property specs, supported entries are PropertyName or PropertyName|FieldName or PropertyName|FieldName(ABL DataType)
       @param pcUniqueKeyProperties The comma delimited list of property names that build the PUK, must be form the list in pcProperties
       @return The handle of the created temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE CreateTempTable (poType AS System.Type,
                                          pcProperties AS CHARACTER,
                                          pcUniqueKeyProperties AS CHARACTER):

        DEFINE VARIABLE cTempTableName AS CHARACTER NO-UNDO.

        ObjectAssert:IsValid (poType, "poType":U) .

        ASSIGN cTempTableName = poType:Name .

        RETURN CreateTempTable (poType,
                                pcProperties,
                                pcUniqueKeyProperties,
                                cTempTableName).

    END METHOD.
&ENDIF

END CLASS.
