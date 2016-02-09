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
    File        : RefactorSession
    Purpose     : Supports refactoring in the sense of replacing the text
                  of individual JPNode's with a provided text. The utility 
                  keeps track of replaced line breaks etc. so that multiple
                  replacements within the same files do not cause the 
                  Parse Tree to be out of sync with the file contents. 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 12 22:42:08 CEST 2015
    Notes       : Supports only processing of JPNode's within a single source
                  file, A single RefactorSession instance can only refactor 
                  once
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN 
USING com.joanju.proparse.NodeTypes         FROM ASSEMBLY .
USING Consultingwerk.Assertion.*            FROM PROPATH .
USING Consultingwerk.Exceptions.*           FROM PROPATH .
USING Consultingwerk.Studio.Proparse.*      FROM PROPATH . 
USING Consultingwerk.Studio.SmartRefactor.* FROM PROPATH .  
USING Consultingwerk.Util.*                 FROM PROPATH .
USING org.prorefactor.core.JPNode           FROM ASSEMBLY .
USING org.prorefactor.treeparser.*          FROM ASSEMBLY .
&ENDIF                                      
USING Progress.Lang.*                       FROM PROPATH .


CLASS Consultingwerk.Studio.SmartRefactor.RefactorSession: 

    DEFINE VARIABLE hFrame    AS HANDLE    NO-UNDO .
    DEFINE VARIABLE hEditor   AS HANDLE    NO-UNDO .
    
    DEFINE TEMP-TABLE ttRefactor NO-UNDO 
        FIELD OffsetFrom      AS INTEGER 
        FIELD OffsetTo        AS INTEGER 
        FIELD ReplaceText     AS CHARACTER 
        FIELD ReplaceOrInsert AS LOGICAL INIT TRUE /* true = replace, false = insert */
        INDEX Offset OffsetFrom OffsetTo .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the file name that is processed by this RefactorSession
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if the RefactorSession has refactor tasks defined
        Notes:   Actually returns of PerformRefactor would actually change the source
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY HasTasks AS LOGICAL NO-UNDO 
	GET:
		RETURN CAN-FIND (FIRST ttRefactor) .
	END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the ParseUnit this RefactorSession is 
                 processing
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ParseUnit AS ParseUnit NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if this RefactorSession instance has already performed the 
                 refactoring
        Notes:   A single RefactorSession instance can only refactor once
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Refactored AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RefactorSession class
        Notes:   
        @param poParseUnit The ProParse Unit to refactor
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RefactorSession (poParseUnit AS ParseUnit):
		SUPER ().
 
        ObjectAssert:IsValid (poParseUnit, "poParseUnit":U) .

        ASSIGN THIS-OBJECT:ParseUnit = poParseUnit 
               THIS-OBJECT:FileName  = poParseUnit:getFile():getPath ().
		
        CREATE FRAME hFrame . 
        
        CREATE EDITOR hEditor 
            ASSIGN WIDTH  = 60 
                   HEIGHT = 20 
                   LARGE  = TRUE 
                   SCROLLBAR-HORIZONTAL = TRUE 
                   SCROLLBAR-VERTICAL   = TRUE 
                   FRAME = hFrame .     
        
        hEditor:READ-FILE (THIS-OBJECT:FileName) .

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Inserts the given text before the given JPNode
        Notes:
        @param poNode The JPNode to replace
        @param pcNewText The new text for the node         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InsertAfterNode (poNode AS JPNode,
                                        pcNewText AS CHARACTER):
        
        IF THIS-OBJECT:Refactored = TRUE THEN
            UNDO, THROW NEW CanOnlyRefactorOnceException ("Cannot add another replacement after code was already refactored."{&TRAN}, 0) .

        /* We only support replacing contents in the actual file */
        Assert:Equals (poNode:getFileName(), THIS-OBJECT:FileName) .

        ObjectAssert:IsValid (poNode, "poNode":U) .

        /* Use editor widget to resolve lin/col into absolute position */
        hEditor:CURSOR-LINE = poNode:getLine() .
        hEditor:CURSOR-CHAR = poNode:getColumn() .

        THIS-OBJECT:InsertRefactorTask (hEditor:CURSOR-OFFSET + LENGTH (poNode:getText()),
                                        hEditor:CURSOR-OFFSET + LENGTH (poNode:getText()),
                                        pcNewText,
                                        FALSE) .

    END METHOD.
    
	/*------------------------------------------------------------------------------
	    Purpose: Inserts the given text before the given JPNode
	    Notes:
        @param poNode The JPNode to replace
        @param pcNewText The new text for the node         
        @param plStartOfLine Logical value indicating if the text should be inserted at the beginning of the line or just before the JPNode position        
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID InsertBeforeNode (poNode AS JPNode,
                                         pcNewText AS CHARACTER,
                                         plStartOfLine AS LOGICAL):
		
        IF THIS-OBJECT:Refactored = TRUE THEN
            UNDO, THROW NEW CanOnlyRefactorOnceException ("Cannot add another replacement after code was already refactored."{&TRAN}, 0) .

        /* We only support replacing contents in the actual file */
        Assert:Equals (poNode:getFileName(), THIS-OBJECT:FileName) .

        ObjectAssert:IsValid (poNode, "poNode":U) .

        /* Use editor widget to resolve lin/col into absolute position */
        hEditor:CURSOR-LINE = poNode:getLine() .
        
        IF plStartOfLine = TRUE THEN 
            hEditor:CURSOR-CHAR = 1 .
        ELSE 
            hEditor:CURSOR-CHAR = poNode:getColumn() .

        THIS-OBJECT:InsertRefactorTask (hEditor:CURSOR-OFFSET,
                                        hEditor:CURSOR-OFFSET,
                                        pcNewText,
                                        FALSE) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Registers a location in the source for replacement
        Notes:   
        @param piOffsetFrom The starting offset to rpelace
        @param piOffsetTo The ending offset to replace
        @param pcNewText The new text to insert
        @param plReplaceOrInsert Logical value if text should be replace the offset (true) or be insertted at the offset (false) 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InsertRefactorTask (piOffsetFrom AS INTEGER,
	                                          piOffsetTo AS INTEGER, 
	                                          pcNewText AS CHARACTER,
	                                          plReplaceOrInsert AS LOGICAL):
	
	    DEFINE BUFFER ttRefactor FOR ttRefactor .

           /* Start of new section within existing section */	
	    IF CAN-FIND (ttRefactor WHERE ttRefactor.OffsetFrom <= piOffsetFrom 
	                              AND ttRefactor.OffsetTo   >= piOffsetFrom) OR
           /* End of new section within existing section */	                                   
           CAN-FIND (ttRefactor WHERE ttRefactor.OffsetFrom <= piOffsetTo
                                  AND ttRefactor.OffsetTo   >= piOffsetTo) OR 
           /* New section overlaps existing section */
           CAN-FIND (ttRefactor WHERE ttRefactor.OffsetFrom >= piOffsetFrom
                                  AND ttRefactor.OffsetTo   <= piOffsetTo) THEN 
            
            UNDO, THROW NEW OverlappingSectionNotSupportedException ("Cannot refactor with overlapping code sections."{&TRAN},
                                                                     0) .                                   	                                 
	
        CREATE ttRefactor.
        ASSIGN ttRefactor.OffsetFrom      = piOffsetFrom
               ttRefactor.OffsetTo        = piOffsetTo
               ttRefactor.ReplaceText     = pcNewText
               ttRefactor.ReplaceOrInsert = plReplaceOrInsert .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the modifications of the file contents 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PerformRefactor ():
		
        IF THIS-OBJECT:Refactored = TRUE THEN 
            UNDO, THROW NEW CanOnlyRefactorOnceException ("Can only refactor once per RefactorSession."{&TRAN}, 0) .		
	
	    FOR EACH ttRefactor BY ttRefactor.OffsetFrom DESCENDING ON ERROR UNDO, THROW:
	
	        IF ttRefactor.ReplaceOrInsert = TRUE THEN DO:
                hEditor:SET-SELECTION (ttRefactor.OffsetFrom,
                                       ttRefactor.OffsetTo + 1) .
        
                hEditor:REPLACE-SELECTION-TEXT (ttRefactor.ReplaceText) .
            END.
            ELSE DO:
                hEditor:CURSOR-OFFSET = ttRefactor.OffsetFrom . 
                hEditor:INSERT-STRING  (ttRefactor.ReplaceText) . 
            END.
	    END.

        FINALLY:
    		ASSIGN THIS-OBJECT:Refactored = TRUE . 
        END FINALLY.		

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Replaces the contents of the given JPNode instance
        Notes:   
        @param poNode The JPNode to replace
        @param pcNewText The new text for the node                 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ReplaceNodeContent (poNode AS JPNode,
	                                       pcNewText AS CHARACTER):
	      
        IF THIS-OBJECT:Refactored = TRUE THEN 
            UNDO, THROW NEW CanOnlyRefactorOnceException ("Cannot add another replacement after code was already refactored."{&TRAN}, 0) .
            
	    /* We only support replacing contents in the actual file */
	    Assert:Equals (poNode:getFileName(), THIS-OBJECT:FileName) .

        ObjectAssert:IsValid (poNode, "poNode":U) .
        
        /* Use editor widget to resolve lin/col into absolute position */
        hEditor:CURSOR-LINE = poNode:getLine() .
        hEditor:CURSOR-CHAR = poNode:getColumn() .

        THIS-OBJECT:InsertRefactorTask (hEditor:CURSOR-OFFSET,
                                        hEditor:CURSOR-OFFSET + LENGTH (poNode:getText ()) - 1,
                                        pcNewText,
                                        TRUE) .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Replaces the contents of the given JPNode instance including all it's 
                 Children
        Notes:   
        @param poNode The JPNode to replace
        @param pcNewText The new text for the node                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ReplaceAllNodeContent (poNode AS JPNode,
                                              pcNewText AS CHARACTER):
          
        DEFINE VARIABLE iEnd      AS INTEGER   NO-UNDO .

        IF THIS-OBJECT:Refactored = TRUE THEN 
            UNDO, THROW NEW CanOnlyRefactorOnceException ("Cannot add another replacement after code was already refactored."{&TRAN}, 0) .    

        /* We only support replacing contents in the actual file */
        Assert:Equals (poNode:getFileName(), THIS-OBJECT:FileName) .

        ObjectAssert:IsValid (poNode, "poNode":U) .

        /* Single Node handled by ReplaceNodeContent() */
        IF NOT VALID-OBJECT (poNode:lastChild()) THEN DO: 
            THIS-OBJECT:ReplaceNodeContent (poNode, pcNewText) .
            
            RETURN .
        END.    
        
        /* Navigate to the end */
        hEditor:CURSOR-LINE = poNode:lastChild():getLine() .
        hEditor:CURSOR-CHAR = poNode:lastChild():getColumn() .
        
        ASSIGN iEnd = hEditor:CURSOR-OFFSET .
        
        /* Use editor widget to resolve lin/col into absolute position */
        hEditor:CURSOR-LINE = poNode:getLine() .
        hEditor:CURSOR-CHAR = poNode:getColumn() .
        
        THIS-OBJECT:InsertRefactorTask (hEditor:CURSOR-OFFSET,
                                        iEnd + LENGTH (poNode:lastChild():getText ()) - 1,
                                        pcNewText,
                                        TRUE) .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves the contents of the RefactSession to the original file name 
        Notes:   
        @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Save ():
        
        RETURN THIS-OBJECT:Save (THIS-OBJECT:FileName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves the contents of the RefactSession to a file 
        Notes:   
        @param pcFileName The name of the file to save to 
        @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL Save (pcFileName AS CHARACTER):
	
	    IF NOT THIS-OBJECT:Refactored THEN 
	       THIS-OBJECT:PerformRefactor () .
		
		RETURN hEditor:SAVE-FILE (pcFileName) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the RefactorSession class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC RefactorSession ():

        GarbageCollectorHelper:DeleteObject (hEditor) .     
        GarbageCollectorHelper:DeleteObject (hFrame) .     
    
    END DESTRUCTOR .

END CLASS.
