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
    File        : QueryExpression
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 10 10:52:28 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*           FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.QueryExpression
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS IQueryExpression:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the First Operand of the Query
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i FirstOperand Consultingwerk.IQueryExpression}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Operator of the QueryExpression
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Operator Consultingwerk.OperatorEnum}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the First Operand of the Query
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i SecondOperand Consultingwerk.IQueryExpression}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param poSecondExpression The right operand of the Query Expression
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        poSecondExpression AS IQueryExpression):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = poSecondExpression .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param pcExpression A Character Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pcExpression AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW CharacterHolder (pcExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param pdtExpression A Date Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATE):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW DateHolder (pdtExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param pdtExpression A DateTime Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW DateTimeHolder (pdtExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param pdtExpression A DateTime Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME-TZ):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW DateTimeTzHolder (pdtExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param pdeExpression A Decimal Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdeExpression AS DECIMAL):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW DecimalHolder (pdeExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param piExpression An Integer Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INTEGER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW IntegerHolder (piExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param piExpression An Int64 Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INT64):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW Int64Holder (piExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param plExpression A Logical Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        plExpression AS LOGICAL):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW LogicalHolder (plExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param pcFieldName The name of the Field to Filter on
        @param poOperator The operator
        @param proExpression A Rowid Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        proExpression AS ROWID):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = NEW BufferFieldName (pcFieldName)
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW RowidHolder (proExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param poSecondExpression The right operand of the Query Expression
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        poSecondExpression AS IQueryExpression):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = poSecondExpression .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pcExpression A Character Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pcExpression AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW CharacterHolder (pcExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pdtExpression A Date Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATE):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW DateHolder (pdtExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pdtExpression A DateTime Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW DateTimeHolder (pdtExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pdtExpression A DateTime Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME-TZ):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW DateTimeTzHolder (pdtExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param pdeExpression A Decimal Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdeExpression AS DECIMAL):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW DecimalHolder (pdeExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param piExpression An Integer Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INTEGER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW IntegerHolder (piExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param piExpression An Int64 Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INT64):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW Int64Holder (piExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param plExpression A Logical Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        plExpression AS LOGICAL):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW LogicalHolder (plExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryExpression class
        Notes:
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator
        @param proExpression A Rowid Expression to as the argument
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC QueryExpression (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        proExpression AS ROWID):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FirstOperand  = poFirstExpression
               THIS-OBJECT:Operator      = poOperator
               THIS-OBJECT:SecondOperand = NEW RowidHolder (proExpression) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Locates a QueryExpression with the given BufferFieldName as the
                 first operand
        Notes:   Recursively traverses ListQueryExpressions
        @param poField The BufferFieldName to locate
        @param poExpression The IQueryExpression to look in for the BufferFieldName
        @return The QueryExpression for the BufferFieldName
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC QueryExpression FindQueryExpressionForBufferField (poField AS BufferFieldName,
                                                                            poExpression AS IQueryExpression):

        DEFINE VARIABLE oExpression AS QueryExpression NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poField "'poField':U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poExpression "'poExpression':U"} .

        IF TYPE-OF (poExpression, QueryExpression) THEN DO:
            oExpression = CAST (poExpression, QueryExpression) .

            IF TYPE-OF (oExpression:FirstOperand, BufferFieldName) THEN DO:

                  IF poField:TableName = CAST (oExpression:FirstOperand, BufferFieldName):TableName AND
                     poField:FieldName = CAST (oExpression:FirstOperand, BufferFieldName):FieldName THEN

                      RETURN oExpression .
            END.
        END.
        ELSE IF TYPE-OF (poExpression, ListQueryExpression) THEN DO:

            {Consultingwerk/foreachABL.i IQueryExpression oEntry in CAST(poExpression,ListQueryExpression)}

                  oExpression = QueryExpression:FindQueryExpressionForBufferField (poField, oEntry) .

                  IF VALID-OBJECT (oExpression) THEN
                      RETURN oExpression.
            END.
        END.

        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Expression to be used in a Query String
        Notes:
        @return The CHARACTER Expression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetExpression ():

        IF THIS-OBJECT:Operator = OperatorEnum:Or THEN
            RETURN SUBSTITUTE ("(&1 &3 &2)":U,
                               THIS-OBJECT:FirstOperand:GetExpression(),
                               THIS-OBJECT:SecondOperand:GetExpression(),
                               THIS-OBJECT:Operator:ToString()) .

        RETURN SUBSTITUTE ("&1 &3 &2":U,
                           THIS-OBJECT:FirstOperand:GetExpression(),
                           THIS-OBJECT:SecondOperand:GetExpression(),
                           THIS-OBJECT:Operator:ToString()) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the expression (QueryExpression) for the the given Buffer Field
                 Name
        Notes:   Based on FindQueryExpressionForBufferField (), returns invalid object
                 reference when the field is not contained
        @param pcFieldName The buffer field name to test
        @param poExpression The IQueryExpression to look in for the BufferFieldName
        @return The expression (QueryExpression) for the the given Buffer Field Name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC QueryExpression GetExpressionForField (pcFieldName AS CHARACTER,
                                                                poExpression AS IQueryExpression):

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcFieldName "'pcFieldName':U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poExpression "'poExpression':U"} .

        RETURN QueryExpression:FindQueryExpressionForBufferField(BufferHelper:ParseFieldName (pcFieldName), poExpression) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given IQueryExpression (typically QueryExpression or
                 ListQueryExpression) contains an expression for the given Buffer Field
                 Name
        Notes:   Based on FindQueryExpressionForBufferField ()
        @param pcFieldName The buffer field name to test
        @param poExpression The IQueryExpression to look in for the BufferFieldName
        @return Logical value indicating if the IQueryExpression contains an expression for the given Buffer Field
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL HasExpressionForField (pcFieldName AS CHARACTER,
                                                        poExpression AS IQueryExpression):

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcFieldName "'pcFieldName':U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poExpression "'poExpression':U"} .

        RETURN VALID-OBJECT (QueryExpression:FindQueryExpressionForBufferField(BufferHelper:ParseFieldName (pcFieldName), poExpression)) .

    END METHOD .

END CLASS.
