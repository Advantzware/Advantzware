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
    File        : UltraGridFilterRowHelper
    Purpose     : Supports building a QueryExpression from the values of the
                  UltraGrid's FilterRow
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 11 02:36:04 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Util.*           FROM PROPATH .
USING Consultingwerk.Windows.Util.*   FROM PROPATH .
USING Infragistics.Win.*              FROM ASSEMBLY .
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .
USING Progress.Lang.*                 FROM PROPATH .
USING Progress.Util.*                 FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Util.UltraGridFilterRowHelper:

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE UltraGridFilterRowHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns an Array of IQueryExpression objects
        Notes:   Defaults to BuildQueryExpression (poGrid, poListQueryExpressions, FALSE, FALSE)
        @param poGrid The source grid
        @param poListQueryExpressions The list of query operators per tables the expression will be added to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID BuildQueryExpression (poGrid AS UltraGrid,
                                                    poListQueryExpressions AS ListQueryExpressionByTable):

        UltraGridFilterRowHelper:BuildQueryExpression (poGrid, poListQueryExpressions, FALSE, FALSE) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an Array of IQueryExpression objects
        Notes:   MATCHES uses a flexible combination with BEGINS based on entered text
                 or BEGINS for character fields
                 Defaults to BuildQueryExpression (poGrid, poListQueryExpressions, plUseMatches, FALSE)
        @param poGrid The source grid
        @param poListQueryExpressions The list of query operators per tables the expression will be added to
        @param plUseMatches Logical value indicating if MATCHES should be used for Character fields
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID BuildQueryExpression (poGrid AS UltraGrid,
                                                    poListQueryExpressions AS ListQueryExpressionByTable,
                                                    plUseMatches AS LOGICAL):

        UltraGridFilterRowHelper:BuildQueryExpression (poGrid, poListQueryExpressions, plUseMatches, FALSE) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an Array of IQueryExpression objects
        Notes:   MATCHES uses a flexible combination with BEGINS based on entered text
                 or BEGINS for character fields

                 Supported UltraGrid FilterOperators per data type (plUseOperators = TRUE):

                 LOGICAL:                     Equals, NotEquals
                 DATE, DATETIME, DATETIME-TZ, Equals, NotEquals, GreaterThan, GreaterThanOrEqualTo,
                 DECIMAL, INTEGER, INT64:     LessThan, LessThanOrEqualTo
                 CHARACTER, LONGCHAR:         Equals, NotEquals, StartsWith, Like, EndsWith,
                                              GreaterThan, GreaterThanOrEqualTo, LessThan, LessThanOrEqualTo
        @param poGrid The source grid
        @param poListQueryExpressions The list of query operators per tables the expression will be added to
        @param plUseMatches Logical value indicating if MATCHES should be used for Character fields
        @param plUseOperators Logical value indicating if the actual filter operator of the filter cell shall be used or a default operator (BEGINS, GE) shall be assumed
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID BuildQueryExpression (poGrid AS UltraGrid,
                                                    poListQueryExpressions AS ListQueryExpressionByTable,
                                                    plUseMatches AS LOGICAL,
                                                    plUseOperators AS LOGICAL):

        DEFINE VARIABLE oFilterRow       AS UltraGridFilterRow   NO-UNDO .
        DEFINE VARIABLE oFilterCell      AS UltraGridFilterCell  NO-UNDO .
        DEFINE VARIABLE cKey             AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE oValue           AS System.Object        NO-UNDO .
        DEFINE VARIABLE oQueryExpression AS QueryExpression      NO-UNDO .
        DEFINE VARIABLE cFieldName       AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE cString          AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE iValue           AS INTEGER              NO-UNDO .
        DEFINE VARIABLE i64Value         AS INT64                NO-UNDO .
        DEFINE VARIABLE lValue           AS LOGICAL              NO-UNDO .
        DEFINE VARIABLE dttValue         AS DATETIME             NO-UNDO .
        DEFINE VARIABLE cHolderType      AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE oOperator        AS OperatorEnum         NO-UNDO .
        DEFINE VARIABLE iIndex           AS INTEGER              NO-UNDO .

        DEFINE VARIABLE hQueryField      AS HANDLE               NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poGrid, "poGrid":U) .
        Consultingwerk.Assertion.ObjectAssert:IsValid (poListQueryExpressions, "poListQueryExpression":U) .

        oFilterRow = poGrid:Rows:FilterRow .

        IF VALID-OBJECT (poGrid:ActiveCell) AND
           poGrid:ActiveCell:IsFilterRowCell AND
           poGrid:ActiveCell:IsInEditMode THEN
            poGrid:PerformAction (UltraGridAction:ExitEditMode) .

        {Consultingwerk/foreach.i UltraGridColumn oColumn in poGrid:DisplayLayout:Bands[0]:Columns}

            ASSIGN cKey        = oColumn:Key
                   oFilterCell = CAST (oFilterRow:Cells[cKey], UltraGridFilterCell)
                   oValue      = oFilterCell:Value .

            IF NOT VALID-OBJECT (oValue) THEN
                NEXT .

            IF plUseOperators THEN
                ASSIGN oOperator = UltraGridFilterRowHelper:OperatorFromUltraGrid (oFilterCell:FilterOperator) .
            ELSE
                ASSIGN oOperator = OperatorEnum:GE .

            IF TYPE-OF (oValue, System.String) THEN DO:
                ASSIGN cString = UNBOX (oValue) .

                IF CharacterType:IsNotNullOrEmpty (cString) THEN DO:

                    IF NOT plUseOperators THEN DO:

                        /* Mike Fechner, Consultingwerk Ltd. 28.10.2015
                           SCL-1085 - Special handling for BEGINSMATCHES */
                        IF plUseMatches THEN DO:

                            ASSIGN iIndex = INDEX (cString, "*":U) .

                            CASE iIndex:
                                WHEN 0 THEN
                                    ASSIGN oOperator = OperatorEnum:Begins .
                                WHEN LENGTH (cString) THEN
                                    ASSIGN cString   = SUBSTRING (cString, 1, iIndex - 1)
                                           oOperator = OperatorEnum:Begins .
                                OTHERWISE DO:
                                    IF SUBSTRING (cString, LENGTH (cString), 1) <> "*":U THEN
                                        ASSIGN cString = cString + "*":U .

                                    ASSIGN oOperator = OperatorEnum:Matches .
                                END.
                            END CASE .
                        END.
                        ELSE
                            ASSIGN oOperator = OperatorEnum:Begins .
                    END.
                    ELSE DO:

                        IF EnumHelper:AreEqual(oFilterCell:FilterOperator, FilterComparisionOperator:EndsWith) AND
                           NOT cString BEGINS "*":U THEN

                            ASSIGN cString = SUBSTITUTE ("*&1":U, cString) .

                    END.
                END.

                oQueryExpression = BuildQueryExpressionForField (poGrid,
                                                                 cKey,
                                                                 "Consultingwerk.CharacterHolder":U,
                                                                 BOX (cString),
                                                                 poListQueryExpressions,
                                                                 oOperator) .
            END.
            ELSE IF TYPE-OF (oValue, System.Int32) THEN DO:
                ASSIGN iValue = UNBOX (oValue) .

                IF iValue <> ? THEN
                    oQueryExpression = BuildQueryExpressionForField (poGrid,
                                                                     cKey,
                                                                     "Consultingwerk.IntegerHolder":U,
                                                                     oValue,
                                                                     poListQueryExpressions,
                                                                     oOperator) .
            END.
            ELSE IF TYPE-OF (oValue, System.Int64) THEN DO:
                ASSIGN i64Value = UNBOX (oValue) .

                IF i64Value <> ? THEN
                    oQueryExpression = BuildQueryExpressionForField (poGrid,
                                                                     cKey,
                                                                     "Consultingwerk.Int64Holder":U,
                                                                     oValue,
                                                                     poListQueryExpressions,
                                                                     oOperator) .
            END.
            ELSE IF TYPE-OF (oValue, System.Decimal) THEN DO:
                ASSIGN iValue = UNBOX (oValue) .

                IF iValue <> ? THEN
                    oQueryExpression = BuildQueryExpressionForField (poGrid,
                                                                     cKey,
                                                                     "Consultingwerk.DecimalHolder":U,
                                                                     oValue,
                                                                     poListQueryExpressions,
                                                                     oOperator) .
            END.
            ELSE IF TYPE-OF (oValue, System.Boolean) THEN DO:
                ASSIGN lValue = UNBOX (oValue) .

                IF lValue <> ? THEN
                    oQueryExpression = BuildQueryExpressionForField (poGrid,
                                                                     cKey,
                                                                     "Consultingwerk.LogicalHolder":U,
                                                                     oValue,
                                                                     poListQueryExpressions,
                                                                     oOperator) .
            END.
            ELSE IF TYPE-OF (oValue, System.DateTime) THEN DO:
                ASSIGN dttValue = DATE (UNBOX (oValue)) .

                IF dttValue <> ? THEN DO:
                    cFieldName = BindingSourceHelper:BindingBufferFieldName (CAST (poGrid:DataSource, Progress.Data.BindingSource),
                                                                             cKey) .

                    hQueryField = Consultingwerk.Util.QueryHelper:GetQueryFieldHandle (CAST (poGrid:DataSource,
                                                                                             Progress.Data.BindingSource):Handle,
                                                                                       cFieldName) .

                    IF VALID-HANDLE (hQueryField) THEN DO:

                        CASE hQueryField:DATA-TYPE:
                            WHEN Consultingwerk.DataTypeEnum:DATE THEN
                                ASSIGN cHolderType = "Consultingwerk.DateHolder":U .
                            WHEN Consultingwerk.DataTypeEnum:DATETIME THEN
                                ASSIGN cHolderType = "Consultingwerk.DateTimeHolder":U .
                            WHEN Consultingwerk.DataTypeEnum:DATETIME-TZ THEN
                                ASSIGN cHolderType = "Consultingwerk.DateTimeHolder":U .
                        END CASE .

                        oQueryExpression = BuildQueryExpressionForField (poGrid,
                                                                         cKey,
                                                                         cHolderType,
                                                                         oValue,
                                                                         poListQueryExpressions,
                                                                         oOperator) .
                    END.
                END.
            END.
        END.

     END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a QueryExpression for a single grid filter field
        Notes:
        @param poGrid The source grid
        @param pcKey The key of the current column
        @param pcHolderType The type of the holder class for the field
        @param poValue The filter column value
        @param poListQueryExpressions The list of query operators per tables the expression will be added to
        @param poOperator The operator to use
        @return The Consultingwerk.QueryExpression for the specified filter field
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC QueryExpression BuildQueryExpressionForField (poGrid AS UltraGrid,
                                                                          pcKey AS CHARACTER,
                                                                          pcHolderType AS CHARACTER,
                                                                          poValue AS System.Object,
                                                                          poListQueryExpressions AS ListQueryExpressionByTable,
                                                                          poOperator AS OperatorEnum):

        DEFINE VARIABLE cFieldName       AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE oBufferFieldName AS BufferFieldName NO-UNDO .
        DEFINE VARIABLE oQueryExpression AS QueryExpression NO-UNDO .


        cFieldName = BindingSourceHelper:BindingBufferFieldName (CAST (poGrid:DataSource, Progress.Data.BindingSource),
                                                                 pcKey) .

        IF CharacterType:IsNullOrEmpty (cFieldName) THEN
            RETURN ? .

        oBufferFieldName = BufferHelper:ParseFieldName (cFieldName) .

        oQueryExpression = NEW QueryExpression () .
        oQueryExpression:FirstOperand = oBufferFieldName .
        oQueryExpression:SecondOperand = DYNAMIC-NEW (pcHolderType) (UNBOX (poValue) ) .
        oQueryExpression:Operator = poOperator .

        IF VALID-OBJECT (oQueryExpression) THEN
            poListQueryExpressions:GetItem (oBufferFieldName:TableName):Add (oQueryExpression) .

        RETURN oQueryExpression .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the OperatorEnum matching the UltraGridFilterCell FilterOperator
        Notes:   Supported UltraGrid FilterOperators per data type (plUseOperators = TRUE):

                 LOGICAL:                     Equals, NotEquals
                 DATE, DATETIME, DATETIME-TZ, Equals, NotEquals, GreaterThan, GreaterThanOrEqualTo,
                 DECIMAL, INTEGER, INT64:     LessThan, LessThanOrEqualTo
                 CHARACTER, LONGCHAR:         Equals, NotEquals, StartsWith, Like, EndsWith,
                                              GreaterThan, GreaterThanOrEqualTo, LessThan, LessThanOrEqualTo

                 This method returns MATCHES for EndsWith. The BuildQueryExpression method
                 is required to convert the filter pattern from "abc" to "*abc"
        @param poFilter The FilterOperator of the UltraGridFilterCell
        @return The resulting OperatorEnum value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC OperatorEnum OperatorFromUltraGrid (poFilter AS FilterComparisionOperator):

        IF EnumHelper:AreEqual(poFilter, FilterComparisionOperator:Equals) THEN
            RETURN OperatorEnum:EQ .
        IF EnumHelper:AreEqual(poFilter, FilterComparisionOperator:NotEquals) THEN
            RETURN OperatorEnum:NE .
        IF EnumHelper:AreEqual(poFilter, FilterComparisionOperator:GreaterThan) THEN
            RETURN OperatorEnum:GT .
        IF EnumHelper:AreEqual(poFilter, FilterComparisionOperator:GreaterThanOrEqualTo) THEN
            RETURN OperatorEnum:GE .
        IF EnumHelper:AreEqual(poFilter, FilterComparisionOperator:LessThan) THEN
            RETURN OperatorEnum:LT .
        IF EnumHelper:AreEqual(poFilter, FilterComparisionOperator:LessThanOrEqualTo) THEN
            RETURN OperatorEnum:LE .
        IF EnumHelper:AreEqual(poFilter, FilterComparisionOperator:StartsWith) THEN
            RETURN OperatorEnum:Begins .
        IF EnumHelper:AreEqual(poFilter, FilterComparisionOperator:Like) THEN
            RETURN OperatorEnum:Matches .
        IF EnumHelper:AreEqual(poFilter, FilterComparisionOperator:EndsWith) THEN
            RETURN OperatorEnum:Matches .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the supported fielter operators
        Notes:   Values depend on the Column's data types
        @param poGrid The source grid
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID InitializeSupportedFilterOperators (poGrid AS UltraGrid):

        {Consultingwerk/foreach.i UltraGridColumn oColumn in poGrid:DisplayLayout:Bands[0]:Columns}

            CASE DataTypeHelper:ToAblDataType (oColumn:DataType):

                WHEN "BLOB":U THEN
                    ASSIGN oColumn:AllowRowFiltering = DefaultableBoolean:False .

                WHEN "LOGICAL":U THEN
                    ASSIGN oColumn:FilterOperatorDropDownItems = CAST (EnumHelper:Or (FilterOperatorDropDownItems:Equals,
                                                                                      FilterOperatorDropDownItems:NotEquals),
                                                                       FilterOperatorDropDownItems)
                           oColumn:FilterOperatorDefaultValue = FilterOperatorDefaultValue:Equals .

                WHEN "DATE":U    OR WHEN "DATETIME":U OR WHEN "DATETIME-TZ":U OR
                WHEN "DECIMAL":U OR WHEN "INTEGER":U  OR WHEN "INT64":U       THEN
                    ASSIGN oColumn:FilterOperatorDropDownItems = CAST (EnumHelperExt:Or (FilterOperatorDropDownItems:Equals,
                                                                                         FilterOperatorDropDownItems:NotEquals,
                                                                                         FilterOperatorDropDownItems:GreaterThan,
                                                                                         FilterOperatorDropDownItems:GreaterThanOrEqualTo,
                                                                                         FilterOperatorDropDownItems:LessThan,
                                                                                         FilterOperatorDropDownItems:LessThanOrEqualTo),
                                                                       FilterOperatorDropDownItems)
                           oColumn:FilterOperatorDefaultValue = FilterOperatorDefaultValue:GreaterThanOrEqualTo .

                WHEN "CHARACTER":U OR WHEN "LONGCHAR":U  THEN
                    ASSIGN oColumn:FilterOperatorDropDownItems = CAST (EnumHelperExt:Or (FilterOperatorDropDownItems:Equals,
                                                                                         FilterOperatorDropDownItems:NotEquals,
                                                                                         FilterOperatorDropDownItems:StartsWith,
                                                                                         FilterOperatorDropDownItems:Like,
                                                                                         FilterOperatorDropDownItems:EndsWith,
                                                                                         FilterOperatorDropDownItems:GreaterThan,
                                                                                         FilterOperatorDropDownItems:GreaterThanOrEqualTo,
                                                                                         FilterOperatorDropDownItems:LessThan,
                                                                                         FilterOperatorDropDownItems:LessThanOrEqualTo),
                                                                       FilterOperatorDropDownItems)
                           oColumn:FilterOperatorDefaultValue = FilterOperatorDefaultValue:StartsWith .
            END CASE .

        END.

    END METHOD.

END CLASS.
