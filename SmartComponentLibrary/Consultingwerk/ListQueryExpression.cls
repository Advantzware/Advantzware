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
    File        : ListQueryExpression
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 10 12:49:17 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH .  
USING Consultingwerk.Framework.Base.*           FROM PROPATH.
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.ListQueryExpression 
    INHERITS GenericList
    IMPLEMENTS IQueryExpression: 

    { Consultingwerk/Framework/Base/GenericList.i IQueryExpression }

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Operator of the QueryExpression
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Operator AS OperatorEnum NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListQueryExpression class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListQueryExpression ():
        SUPER ().
        
        THIS-OBJECT:Operator = OperatorEnum:And .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator 
        @param poSecondExpression The right operand of the Query Expression
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        poSecondExpression AS IQueryExpression):
        
        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     poSecondExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator 
        @param pcExpression A Character Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pcExpression AS CHARACTER):
        
        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pcExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator 
        @param pdtExpression A Date Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATE):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator 
        @param pdtExpression A DateTime Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator 
        @param pdtExpression A DateTime Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME-TZ):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator 
        @param pdeExpression A Decimal Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        pdeExpression AS DECIMAL):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     pdeExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator 
        @param piExpression An Integer Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INTEGER):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     piExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator 
        @param piExpression An Int64 Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INT64):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     piExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param pcFieldName The field name to be used as the left operand of the Query Expression
        @param poOperator The operator 
        @param plExpression A Logical Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (pcFieldName AS CHARACTER,
                                        poOperator AS OperatorEnum,
                                        plExpression AS LOGICAL):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (pcFieldName,
                                                     poOperator,
                                                     plExpression)) .
    END METHOD.    

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator 
        @param pcExpression A Character Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pcExpression AS CHARACTER):
        
        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pcExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator 
        @param pdtExpression A Date Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATE):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator 
        @param pdtExpression A DateTime Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pdtExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator 
        @param pdtExpression A DateTime Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdtExpression AS DATETIME-TZ):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pdtExpression)) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator 
        @param pdeExpression A Decimal Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        pdeExpression AS DECIMAL):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     pdeExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator 
        @param piExpression An Integer Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INTEGER):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     piExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator 
        @param piExpression An Int64 Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        piExpression AS INT64):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     piExpression)) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a QueryExpression to the List
        Notes:   
        @param poFirstExpression The left operand of the Query Expression
        @param poOperator The operator 
        @param plExpression A Logical Expression to as the argument
        @return The reference to the new QueryExpression
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression Add (poFirstExpression AS IQueryExpression,
                                        poOperator AS OperatorEnum,
                                        plExpression AS LOGICAL):

        RETURN THIS-OBJECT:Add (NEW QueryExpression (poFirstExpression,
                                                     poOperator,
                                                     plExpression)) .
    END METHOD.    
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the Expression to be used in a Query String
        Notes:   
        @return The CHARACTER Expression  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetExpression ():
        
        DEFINE VARIABLE cExpression AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOperator   AS CHARACTER NO-UNDO.
        
        IF NOT VALID-OBJECT (THIS-OBJECT:Operator) THEN
            THIS-OBJECT:Operator = OperatorEnum:And . 
        
        ASSIGN cOperator = THIS-OBJECT:Operator:ToString() .
        
        {Consultingwerk/foreachABL.i IQueryExpression oExpression in THIS-OBJECT}
        
            IF cExpression > "":U THEN 
                cExpression = SUBSTITUTE ("&1 &3 &2":U, 
                                          cExpression,
                                          THIS-OBJECT:GetExpressionForEntry (oExpression),
                                          cOperator) .
            ELSE 
                cExpression = THIS-OBJECT:GetExpressionForEntry (oExpression) .
        END.

/*        IF THIS-OBJECT:Operator <> OperatorEnum:And THEN*/
/*            RETURN SUBSTITUTE ("(&1)":U, cExpression) . */

        RETURN cExpression . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the expression value for an entry in the List 
        Notes:   Adds parenthesis for Lists that are entries in this List
        @param poExpression The expression to return the value for 
        @return The value for the given expression
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER GetExpressionForEntry (poExpression AS IQueryExpression):
		
		IF TYPE-OF (poExpression, IEnumerable) THEN 
		    RETURN SUBSTITUTE ("(&1)":U, poExpression:GetExpression()) .
		ELSE 
		    RETURN poExpression:GetExpression() . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the ListQueryExpression is empty
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IsEmpty AS LOGICAL NO-UNDO 
    GET():
        
        IF THIS-OBJECT:Count > 0 THEN 
            RETURN FALSE . 
        ELSE         
            RETURN TRUE . 
        
    END GET.

END CLASS.
