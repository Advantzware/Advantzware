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
    File        : CaseStatementParser
    Purpose     : A Parser for CASE Statements
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 28 10:10:57 CET 2013
    Notes       : Currently the Parser does not support WHEN ... OR WHEN ... style
                  of options. 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
USING com.joanju.proparse.NodeTypes    FROM ASSEMBLY .
USING Consultingwerk.Studio.*          FROM PROPATH .  
USING Consultingwerk.Studio.Proparse.* FROM PROPATH .  
USING Consultingwerk.Util.*            FROM PROPATH .  
USING org.prorefactor.core.JPNode      FROM ASSEMBLY .
USING org.prorefactor.treeparser.*     FROM ASSEMBLY .
USING Progress.Lang.*                  FROM PROPATH .
&ENDIF

CLASS Consultingwerk.Studio.CaseStatementParser: 

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE VARIABLE oCaseNode AS JPNode NO-UNDO .  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CaseStatementParser class
        Notes:   
        @param poNode The reference to the JPNode to parse
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CaseStatementParser (poNode AS JPNode):
		SUPER ().
		
		Consultingwerk.Assertion.ProparseAssert:NodeType (poNode, "CASE":U) .
		
		ASSIGN oCaseNode = poNode . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the JPNode representing the END or END CASE
                 Statement  
        Notes:   
        @return The reference to the JPNode representing the END or END CASE Statement 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC JPNode GetEndCaseNode ():
		
		RETURN ProparseHelper:FindChildNodeOfNodeType (oCaseNode, "END":U) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of the CaseWhen optins for the CASE Statement 
        Notes:   
        @return The List of the CaseWhen options of the CASE Statement
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ListCaseWhen GetListCaseWhen ():
		
		DEFINE VARIABLE oCodeBlock AS JPNode       NO-UNDO . 
        DEFINE VARIABLE oWhenBlock AS JPNode       NO-UNDO . 
		DEFINE VARIABLE oList      AS ListCaseWhen NO-UNDO .
		
		ASSIGN oList = NEW ListCaseWhen () .
		
		oCodeBlock = ProparseHelper:FindChildNodeOfNodeType (oCaseNode, "Code_block":U) .

        /* Now loop through the WHEN Nodes */
        ASSIGN oWhenBlock = oCodeBlock:firstChild() . 

        DO WHILE VALID-OBJECT (oWhenBlock) ON ERROR UNDO, THROW:
            IF NodeTypes:getTypeName(oWhenBlock:getType()) = "WHEN":U THEN
                  THIS-OBJECT:ParseWhenBlock (oWhenBlock, oList) .
            
            oWhenBlock = oWhenBlock:nextSibling () .
        END.
        
        RETURN oList . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the JPNode representing the OTHERWISE keyword
                 
        Notes:   
        @return The reference to the JPNode representing the OTHERWISE keyword  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC JPNode GetOtherwiseNode ():
        
        RETURN ProparseHelper:FindChildNodeOfNodeType (oCaseNode, "OTHERWISE":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses a WHEN option of a CASE Statement
        Notes:   Removes quotes from the Expression following the WHEN keyword   
        @param poWhenNode The JPNode representing the WHEN key word
        @param poList The List of CaseWhen options 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ParseWhenBlock (poWhenNode AS JPNode,
	                                      poList AS ListCaseWhen):
		
		DEFINE VARIABLE oQStringNode AS JPNode NO-UNDO . 
		
		oQStringNode = ProparseHelper:FindChildNodeOfNodeType (poWhenNode, "QSTRING":U) .
		
		poList:Add (StringHelper:Unquote(oQStringNode:getText ()), 
                    poWhenNode:getFilename (),
                    poWhenNode:getLine (),
                    poWhenNode:getColumn (),
                    poWhenNode) .

	END METHOD .
&ENDIF
END CLASS.
