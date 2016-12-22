/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    Purpose     : 
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
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .  
USING Progress.Lang.*                 FROM PROPATH .

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
        Notes:   
        @param poGrid The source grid
        @param poListQueryExpressions The list of query operators per tables the expression will be added to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID BuildQueryExpression (poGrid AS UltraGrid,
                                                    poListQueryExpressions AS ListQueryExpressionByTable):

        UltraGridFilterRowHelper:BuildQueryExpression (poGrid, poListQueryExpressions, FALSE) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns an Array of IQueryExpression objects 
        Notes:   MATCHES uses a flexible combination with BEGINS based on entered text
                 or BEGINS for character fields
        @param poGrid The source grid
        @param poListQueryExpressions The list of query operators per tables the expression will be added to
        @param plUseMatches Logical value indicating if MATCHES should be used for Character fields
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID BuildQueryExpression (poGrid AS UltraGrid,
                                                    poListQueryExpressions AS ListQueryExpressionByTable,
                                                    plUseMatches AS LOGICAL):
    
        DEFINE VARIABLE oFilterRow       AS UltraGridFilterRow   NO-UNDO . 
        DEFINE VARIABLE oFilterCell      AS UltraGridFilterCell  NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER              NO-UNDO .
        DEFINE VARIABLE cKey             AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE oValue           AS System.Object        NO-UNDO .
        DEFINE VARIABLE oQueryExpression AS QueryExpression      NO-UNDO . 
        DEFINE VARIABLE cTableName       AS CHARACTER            NO-UNDO .        
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

            IF NOT VALID-OBJECT (oValue) THEN NEXT .

            IF TYPE-OF (oValue, System.String) THEN DO:
                ASSIGN cString = UNBOX (oValue) .

                IF CharacterType:IsNotNullOrEmpty (cString) THEN DO:

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

                    oQueryExpression = BuildQueryExpressionForField (poGrid, 
                                                                     cKey,
                                                                     "Consultingwerk.CharacterHolder":U,
                                                                     BOX (cString),
                                                                     poListQueryExpressions,
                                                                     oOperator) .
                END.                                                                     
            END.
            ELSE IF TYPE-OF (oValue, System.Int32) THEN DO:
                ASSIGN iValue = UNBOX (oValue) .
                
                IF iValue <> ? THEN 
                    oQueryExpression = BuildQueryExpressionForField (poGrid, 
                                                                     cKey,
                                                                     "Consultingwerk.IntegerHolder":U,
                                                                     oValue,
                                                                     poListQueryExpressions,
                                                                     OperatorEnum:GE) .
            END.
            ELSE IF TYPE-OF (oValue, System.Int64) THEN DO:
                ASSIGN i64Value = UNBOX (oValue) .

                IF i64Value <> ? THEN
                    oQueryExpression = BuildQueryExpressionForField (poGrid,
                                                                     cKey,
                                                                     "Consultingwerk.Int64Holder":U,
                                                                     oValue,
                                                                     poListQueryExpressions,
                                                                     OperatorEnum:GE) .
            END.
            ELSE IF TYPE-OF (oValue, System.Decimal) THEN DO:
                ASSIGN iValue = UNBOX (oValue) .
                
                IF iValue <> ? THEN 
                    oQueryExpression = BuildQueryExpressionForField (poGrid, 
                                                                     cKey,
                                                                     "Consultingwerk.DecimalHolder":U,
                                                                     oValue,
                                                                     poListQueryExpressions,
                                                                     OperatorEnum:GE) .
            END.
            ELSE IF TYPE-OF (oValue, System.Boolean) THEN DO:
                ASSIGN lValue = UNBOX (oValue) .
                
                IF lValue <> ? THEN 
                    oQueryExpression = BuildQueryExpressionForField (poGrid, 
                                                                     cKey,
                                                                     "Consultingwerk.LogicalHolder":U,
                                                                     oValue,
                                                                     poListQueryExpressions,
                                                                     OperatorEnum:EQ) .
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
                                                                         OperatorEnum:GE) .
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
         
END CLASS.
