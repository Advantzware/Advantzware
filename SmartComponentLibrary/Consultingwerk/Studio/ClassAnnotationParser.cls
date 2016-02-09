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
    File        : ClassAnnotationParser
    Purpose     : Parser for class and class member annotations
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 20 20:45:02 CET 2013
    Notes       : This class uses proparse and is thus dependent on 
                  windows / the .NET framework
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i} 

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
USING com.joanju.proparse.NodeTypes    FROM ASSEMBLY .
USING Consultingwerk.*                 FROM PROPATH . 
USING Consultingwerk.Studio.*          FROM PROPATH .
USING Consultingwerk.Studio.Proparse.* FROM PROPATH .
USING Consultingwerk.Util.*            FROM PROPATH . 
USING org.prorefactor.core.JPNode      FROM ASSEMBLY .
USING org.prorefactor.treeparser.*     FROM ASSEMBLY .
USING Progress.Lang.*                  FROM PROPATH .
&ENDIF

CLASS Consultingwerk.Studio.ClassAnnotationParser
&IF DEFINED (DotNetAccessible) NE 0 &THEN 
    IMPLEMENTS IClassAnnotationParser
&ENDIF        
    : 

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
    DEFINE VARIABLE oParseUnit AS ParseUnit NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassAnnotationParser class
        Notes:   
        @param pcFileName The file name to parse
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ClassAnnotationParser (pcFileName AS CHARACTER):
		SUPER ().
		
        ProparseHelper:SetProparseEnvironment() . 
        ProparseHelper:ExportDatabaseSchema () .
        
        ProparseHelper:Initialize() .       
                
        oParseUnit = ProparseHelper:ParseFile (pcFileName) .                
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds an Array of Annotations to the ClassMember
        Notes:   
        @param poMember The ClassMember instance to add the annotations to 
        @param poAnnotations The Annotation Array to add to the ClassMember
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID AddAnnotations (poMember AS ClassMember,
	                                      poAnnotations AS Annotation EXTENT):
		
		DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        DO i = 1 TO EXTENT (poAnnotations):
            IF VALID-OBJECT (poAnnotations[i]) THEN 
                poMember:Annotations:Add (poAnnotations[i]) .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets the List of Annotations of the ClassHeader
        Notes:   
        @return The ListAnnotation with the list of annotations from the class header 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ListAnnotation GetClassAnnotations ():

        DEFINE VARIABLE oList AS ListAnnotation NO-UNDO .
        DEFINE VARIABLE oNode AS JPNode         NO-UNDO .

        oList = NEW ListAnnotation () .

        oNode = oParseUnit:getTopNode():firstchild() .

        DO WHILE VALID-OBJECT (oNode):
    
            IF NodeTypes:getTypeName(oNode:getType()) = "ANNOTATION":U THEN 
                
                oList:Add (oNode:getFilename (),
                           oNode:getLine (),
                           oNode:getColumn (),
                           oNode) .

            IF NodeTypes:getTypeName(oNode:getType()) = "CLASS":U THEN 
                LEAVE . 
            
            oNode = oNode:nextSibling () .
        END.

        RETURN oList .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets the List of Class Members and their Annotations
        Notes:   
        @return The ListClassMember with the lsit of class members and their annotations
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ListClassMember GetClassMemberAnnotations ():
		
        DEFINE VARIABLE oList        AS ListClassMember NO-UNDO .
        DEFINE VARIABLE oNode        AS JPNode          NO-UNDO .
        DEFINE VARIABLE oAnnotations AS Annotation      NO-UNDO EXTENT 20 . 
        DEFINE VARIABLE iAnnotation  AS INTEGER         NO-UNDO INIT 0 .
        DEFINE VARIABLE oMember      AS ClassMember     NO-UNDO  .

        oList = NEW ListClassMember () .

        oNode = oParseUnit:getTopNode() .

        /* Find the CLASS Node */    
        oNode = ProparseHelper:FindChildNodeOfNodeType (oNode, "CLASS":U) .    

        IF NOT VALID-OBJECT (oNode) THEN 
            RETURN oList .    

        /* Find the Code_block Node */    
        oNode = ProparseHelper:FindChildNodeOfNodeType (oNode, "Code_block":U) .    

        IF NOT VALID-OBJECT (oNode) THEN 
            RETURN oList .    

        oNode = oNode:firstChild () .

        DO WHILE VALID-OBJECT (oNode):
            
            IF NodeTypes:getTypeName(oNode:getType()) = "ANNOTATION":U THEN DO:
                ASSIGN iAnnotation = iAnnotation + 1
                       oAnnotations [iAnnotation] = NEW Annotation (oNode) .
            END.
            ELSE IF iAnnotation > 0 AND NodeTypes:getTypeName(oNode:getType()) = "CONSTRUCTOR":U THEN DO:

                oMember = NEW ClassMember () .
                
                ASSIGN oMember:MemberType = ClassMemberTypeEnum:Constructor 
                       oMember:Name       = ProparseHelper:FindChildNodeOfNodeType (oNode, "TYPE_NAME":U):getText ()
                       oMember:Public     = ProparseHelper:HasChildNodeOfNodeType (oNode, "PUBLIC":U)
                       oMember:Protected  = ProparseHelper:HasChildNodeOfNodeType (oNode, "PROTECTED":U)
                       oMember:Private    = ProparseHelper:HasChildNodeOfNodeType (oNode, "PRIVATE":U)
                       oMember:Static     = ProparseHelper:HasChildNodeOfNodeType (oNode, "STATIC":U)
                       oMember:Parameters = ProparseHelper:ParseParameterList (oNode).
            
                THIS-OBJECT:AddAnnotations (oMember, oAnnotations) .
                
                ASSIGN oAnnotations = ? 
                       iAnnotation  = 0 . 
            
                oList:Add (oMember) .
            END.
            ELSE IF iAnnotation > 0 AND NodeTypes:getTypeName(oNode:getType()) = "METHOD":U THEN DO:

                oMember = NEW ClassMember () .
            
                ASSIGN oMember:MemberType = ClassMemberTypeEnum:Method
                       oMember:Name       = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText ()
                       oMember:Public     = ProparseHelper:HasChildNodeOfNodeType (oNode, "PUBLIC":U)
                       oMember:Protected  = ProparseHelper:HasChildNodeOfNodeType (oNode, "PROTECTED":U)
                       oMember:Private    = ProparseHelper:HasChildNodeOfNodeType (oNode, "PRIVATE":U)
                       oMember:Static     = ProparseHelper:HasChildNodeOfNodeType (oNode, "STATIC":U)
                       oMember:Parameters = ProparseHelper:ParseParameterList (oNode).
                       .
            
                THIS-OBJECT:AddAnnotations (oMember, oAnnotations) .
                
                ASSIGN oAnnotations = ? 
                       iAnnotation  = 0 . 
            
                oList:Add (oMember) .
            END.
            ELSE IF iAnnotation > 0 AND NodeTypes:getTypeName(oNode:getType()) = "DEFINE":U   
                                    AND ProparseHelper:HasChildNodeOfNodeType (oNode, "PROPERTY":U) THEN DO:

                oMember = NEW ClassMember () .
            
                ASSIGN oMember:MemberType = ClassMemberTypeEnum:Property
                       oMember:Name       = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText ()
                       oMember:Public     = ProparseHelper:HasChildNodeOfNodeType (oNode, "PUBLIC":U)
                       oMember:Protected  = ProparseHelper:HasChildNodeOfNodeType (oNode, "PROTECTED":U)
                       oMember:Private    = ProparseHelper:HasChildNodeOfNodeType (oNode, "PRIVATE":U)
                       oMember:Static     = ProparseHelper:HasChildNodeOfNodeType (oNode, "STATIC":U)
                       .
            
                THIS-OBJECT:AddAnnotations (oMember, oAnnotations) .
                
                ASSIGN oAnnotations = ? 
                       iAnnotation  = 0 . 
            
                oList:Add (oMember) .
            END.
            ELSE IF iAnnotation > 0 AND NodeTypes:getTypeName(oNode:getType()) = "DEFINE":U   
                                    AND ProparseHelper:HasChildNodeOfNodeType (oNode, "EVENT":U) THEN DO:

                oMember = NEW ClassMember () .
            
                ASSIGN oMember:MemberType = ClassMemberTypeEnum:Event
                       oMember:Name       = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U):getText ()
                       oMember:Public     = ProparseHelper:HasChildNodeOfNodeType (oNode, "PUBLIC":U)
                       oMember:Protected  = ProparseHelper:HasChildNodeOfNodeType (oNode, "PROTECTED":U)
                       oMember:Private    = ProparseHelper:HasChildNodeOfNodeType (oNode, "PRIVATE":U)
                       oMember:Static     = ProparseHelper:HasChildNodeOfNodeType (oNode, "STATIC":U)
                       .

                IF ProparseHelper:HasChildNodeOfNodeType (oNode, "SIGNATURE":U) THEN 
                    ASSIGN oMember:Parameters = ProparseHelper:ParseParameterList 
                                                (ProparseHelper:FindChildNodeOfNodeType (oNode, "SIGNATURE":U)).
            
                THIS-OBJECT:AddAnnotations (oMember, oAnnotations) .
                
                ASSIGN oAnnotations = ? 
                       iAnnotation  = 0 . 
            
                oList:Add (oMember) .
            END.

            oNode = oNode:nextSibling () .
        END.

		RETURN oList .

	END METHOD .
&ENDIF
END CLASS.
