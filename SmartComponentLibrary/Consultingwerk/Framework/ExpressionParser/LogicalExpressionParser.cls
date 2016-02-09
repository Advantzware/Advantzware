/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LogicalExpressionParser
    Purpose     : Parses a logical expression
    Syntax      : 
    Description : Returns true or false for the given expression
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 21 10:20:33 CEST 2014
    Notes       : Parses the logical expression and splits it into individual 
                  value expressions. Supports & for AND, | for OR and ! for 
                  NOT. Supports parenthesis between individual terms:
                  (A|B)&(C|D) will turn into (A OR B) AND (C OR D)
                  Parenthesis will be threated as part of an invividual 
                  expression when a non " ", &, |, !, (, ) character has 
                  started the individual expression. So
                  (A|B())&(C(test)|D) will turn into
                  (A OR B()) AND (C(test) OR D) where A, B(), C(test) AND D
                  will be passed as value expressions to the logical value
                  provider. 
                  The Logical value provider is expected to return TRUE or 
                  FALSE based on the given values
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                  FROM PROPATH .
USING Consultingwerk.Framework.ExpressionParser.* FROM PROPATH .  
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Framework.ExpressionParser.LogicalExpressionParser: 

    DEFINE STATIC TEMP-TABLE ttEval NO-UNDO 
        FIELD cDummy AS CHARACTER . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the value provider used by this instance
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY LogicalValueProvider AS ILogicalValueProvider NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Static Constructor for the LogicalExpressionParser class
        Notes:   Creates single temp-table record required by expression parser
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR STATIC LogicalExpressionParser ():
    
        CREATE ttEval .		
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LogicalExpressionParser class
        Notes:   
        @param poValueProvider The ILogicalValueProvider that provides the values for the expression parser
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC LogicalExpressionParser (poValueProvider AS ILogicalValueProvider):
		SUPER ().
		
		{Consultingwerk/Assertion/ObjectAssert/IsValid.i poValueProvider """poValueProvider"":U"} .
		
		ASSIGN THIS-OBJECT:LogicalValueProvider = poValueProvider .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Evaluates a logical expression
        Notes:   Parses logical expressions, supports the following logical operators:
                 & AND
                 | OR
                 ! NOT
                 Supports for double/single quoted expressions
                 Supports parenthesis
        @param pcExpression The logical expression to evaluate
        @return The expression result
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL Evaluate (pcExpression AS CHARACTER):
		
		DEFINE VARIABLE cTargetExpression AS CHARACTER NO-UNDO .
		DEFINE VARIABLE iPos              AS INTEGER   NO-UNDO INIT 1.
        DEFINE VARIABLE iBracket          AS INTEGER   NO-UNDO .
		DEFINE VARIABLE iLength           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cChar             AS CHARACTER NO-UNDO .
		
		DEFINE VARIABLE lInExpression     AS LOGICAL   NO-UNDO INIT FALSE .
		DEFINE VARIABLE cExpression       AS CHARACTER NO-UNDO .
		
		ASSIGN iLength = LENGTH (pcExpression) .
		
		charloop:
		REPEAT ON ERROR UNDO, THROW:		    
		    ASSIGN cChar = SUBSTRING (pcExpression, iPos, 1) .
		    
		    /* When we are currently not in an expression, we expect: 
		        SPACE   - skip 
		        &, |, ! - replace with AND, OR, NOT 
		        ( ) */
		    
		    IF NOT lInExpression THEN DO:
    		    CASE cChar:
    		        WHEN " ":U THEN DO:
    		            ASSIGN cTargetExpression = cTargetExpression + " ":U . 
    		            NEXT charloop .
    		        END.
                    WHEN "&":U THEN DO:
                        ASSIGN cTargetExpression = cTargetExpression + " AND ":U . 
                        NEXT charloop .
                    END.
                    WHEN "|":U THEN DO:
                        ASSIGN cTargetExpression = cTargetExpression + " OR ":U . 
                        NEXT charloop .
                    END.
                    WHEN "!":U THEN DO:
                        ASSIGN cTargetExpression = cTargetExpression + " NOT ":U . 
                        NEXT charloop .
                    END.
                    WHEN "(":U THEN DO:
                        ASSIGN cTargetExpression = cTargetExpression + "(":U . 
                        NEXT charloop .
                    END.
                    WHEN ")":U THEN DO:
                        ASSIGN cTargetExpression = cTargetExpression + ")":U . 
                        NEXT charloop .
                    END.
                    OTHERWISE 
                        IF lInExpression = FALSE THEN 
                            ASSIGN lInExpression = TRUE 
                                   cExpression   = "":U . 
    		    END.
		    END.
		    
	       	/* We're in an expression now. add characters until the next SPACE & | ! ) */
		    IF cChar = " ":U OR cChar = "&":U OR cChar = "|":U OR cChar = "!":U OR cChar = ")":U OR 
		       (cExpression = "":U AND cChar = "(":U) THEN DO:

		        ASSIGN cTargetExpression = cTargetExpression +
		                                   STRING (THIS-OBJECT:LogicalValueProvider:GetLogicalValue (cExpression)) .

		        /* parse breaking char in next iteration */
		        ASSIGN iPos          = iPos - 1 
		               cExpression   = "":U
		               lInExpression = FALSE .
		        
		        NEXT charloop .
		    END.
		    
		    /* We're in an expression now and encountering an opening bracket */
		    IF cChar = "(":U THEN DO:
		    
		      ASSIGN iBracket    = StringHelper:FindClosingBracket (pcExpression, iPos) 
		             cExpression = cExpression + SUBSTRING (pcExpression, iPos, iBracket - iPos + 1) 
		             iPos        = iBracket .
		             
	          NEXT charloop .	             
		    
		    END.
		    
		    ASSIGN cExpression = cExpression + cChar . 
		    
		    FINALLY: 
    		    ASSIGN iPos = iPos + 1.
    
                IF iPos > iLength THEN 
                    LEAVE charloop . 
            END FINALLY.
		END.
		
		IF cExpression > "":U THEN 
            ASSIGN cTargetExpression = cTargetExpression + 
                                       STRING (THIS-OBJECT:LogicalValueProvider:GetLogicalValue (cExpression)) .

		BUFFER ttEval:FIND-FIRST ("WHERE ":U + cTargetExpression) NO-ERROR . 
		
		RETURN BUFFER ttEval:AVAILABLE .
		
	END METHOD .

END CLASS.
