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
    File        : AnnotationParser
    Purpose     : Parses the source code searching for certain annotations
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 27 22:13:06 CET 2013
    Notes       : This class requires Proparse and is only accessible on 
                  Windows
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING com.joanju.proparse.NodeTypes    FROM ASSEMBLY .
USING Consultingwerk.Studio.*          FROM PROPATH .  
USING Consultingwerk.Studio.Proparse.* FROM PROPATH .
USING org.prorefactor.core.JPNode      FROM ASSEMBLY .
USING org.prorefactor.treeparser.*     FROM ASSEMBLY .
USING Progress.Lang.*                  FROM PROPATH .
&ENDIF

CLASS Consultingwerk.Studio.AnnotationParser: 

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE VARIABLE oParseUnit AS ParseUnit NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AnnotationParser class
        Notes:   
        @param pcFileName The file name to parse
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC AnnotationParser (pcFileName AS CHARACTER):
		SUPER ().
		
        ProparseHelper:SetProparseEnvironment() . 
        ProparseHelper:ExportDatabaseSchema () .
        
        ProparseHelper:Initialize() .		
        		
        oParseUnit = ProparseHelper:ParseFile (pcFileName) .        		
        		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Locates Annotations with the specified name
        Notes:   
        @param pcName The name of the Annotation to locate
        @return The ListAnnotation with the list of the annotations
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ListAnnotation FindAnnotations (pcName AS CHARACTER):
		
		DEFINE VARIABLE oList AS ListAnnotation NO-UNDO.

        oList = NEW ListAnnotation () .

        /* When annotation name does not begin with @ then add @ */ 
        IF NOT pcName BEGINS "@":U THEN 
            pcName = "@":U + pcName .
        
        THIS-OBJECT:WalkAst (oParseUnit:getTopNode():firstchild(),
                             pcName,
                             oList) . 

		RETURN oList .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Walks the AST of the ParseUnit and searchs for Annotations with the 
                 given name 
        Notes:   
        @param poASTNode The current parent node
        @param pcName The name of the annotation to locate
        @param poList The ListAnnotation with the list to add the annotations to 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID WalkAst (poASTNode AS JPNode,
	                               pcName AS CHARACTER,
	                               poList AS ListAnnotation):
		
        DO WHILE VALID-OBJECT (poASTNode):
    
            IF NodeTypes:getTypeName(poASTNode:getType()) = "ANNOTATION":U AND 
               poASTNode:getText() = pcName THEN 
                
                poList:Add (poASTNode:getFilename (),
                            poASTNode:getLine (),
                            poASTNode:getColumn (),
                            poASTNode) .
    
            IF VALID-OBJECT (poASTNode:firstChild()) THEN 
                THIS-OBJECT:WalkAST (poASTNode:firstChild(),
                                     pcName,
                                     poList) .
            
            poASTNode = poASTNode:nextSibling () .
        END.


	END METHOD .
&ENDIF
END CLASS.
