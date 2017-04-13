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
    File        : find-nonblock-triggers.p
    Purpose     : Scans the files in the passed in System.String[] for 
                  trigger blocks that do not contain a block (single 
                  statement triggers or triggers containing a RUN statement) 

    Syntax      :

    Description : This file is part of the WinKit MTK

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 29 22:35:47 CEST 2011
    Notes       : Triggers that do not contain a block need to be turned 
                  into a block before the insert-trigger-end-include.p is
                  executed. Only triggers that contain a block can be 
                  refactored to that they contain the triggerend.i include
    @param poFiles A System.String[] containing the list of files to scan
    @param pcOutputFile The name of the XML File to save the temp-table to
    @param pcLogFile The name of the log file used to trace errors    
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Studio.Proparse.* FROM PROPATH .
USING org.prorefactor.refactor.* FROM ASSEMBLY.
USING org.prorefactor.treeparser.* FROM ASSEMBLY.
USING org.prorefactor.core.* FROM ASSEMBLY.
USING com.joanju.proparse.NodeTypes FROM ASSEMBLY .

DEFINE INPUT PARAMETER poFiles AS "System.String[]":U NO-UNDO .
DEFINE INPUT  PARAMETER pcOutputFile AS CHARACTER         NO-UNDO .
DEFINE INPUT  PARAMETER pcLogFile    AS CHARACTER         NO-UNDO .

DEFINE VARIABLE cFile      AS CHARACTER NO-UNDO. 

DEFINE VARIABLE oParseUnit AS ParseUnit NO-UNDO . 

DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE STREAM err .
 
{tools/winkit/ttTrigger.i}

/* ***************************  Main Block  *************************** */

DEFAULT-WINDOW:WIDTH = 320  .
DEFAULT-WINDOW:HEIGHT = 30  .
DEFAULT-WINDOW:TITLE = pcOutputFile .

ProparseHelper:SetProparseEnvironment() . 
ProparseHelper:ExportDatabaseSchema () .

OUTPUT STREAM err TO VALUE (pcLogFile) UNBUFFERED .
ProparseHelper:Initialize() .  
{Consultingwerk/foreach.i System.Object oFile in poFiles}
        
    PAUSE 0 BEFORE-HIDE .         
    ASSIGN i     = i + 1 
           cFile = UNBOX (oFile) . 

    DISPL i poFiles:Length cFile FORMAT "x(100)":U WITH DOWN WIDTH 300.  
/*    PUT STREAM err UNFORMATTED "Processing (":U STRING(i) "): ":U STRING (cFile) SKIP.*/
    DOWN .
    PROCESS EVENTS . 

    DO ON ERROR UNDO, THROW:
        oParseUnit = Consultingwerk.Studio.Proparse.ProparseHelper:ParseFile (cFile) .

        CATCH err AS Progress.Lang.Error:
          
            PUT STREAM err UNFORMATTED STRING (cFile) ":":U TRIM (SUBSTRING (err:GetMessage(1), 1, 2000)) SKIP (1). 
            NEXT .
        END CATCH.
    END.

    RUN WalkAST (oParseUnit:getTopNode():firstchild()) .
END.
TEMP-TABLE ttTrigger:WRITE-XML ("FILE":U, pcOutputFile, TRUE) .

CATCH err AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
END CATCH.

/* **********************  Internal Procedures  *********************** */

PROCEDURE ProcessTriggerBlock:
/*------------------------------------------------------------------------------
	Purpose: Processes the JPNode that represents a Trigger and checks if it contains
	         a block, a single statement or a RUN (PERSISTENT) statement. 																	  
	Notes:  	
    @param poASTNode The current JPNode                               
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poASTNode AS JPNode NO-UNDO . 

    DEFINE VARIABLE oChild      AS JPNode NO-UNDO .
    DEFINE VARIABLE oGrandChild AS JPNode NO-UNDO .
    DEFINE VARIABLE oGrandChild2 AS JPNode    NO-UNDO . 
    
    DEFINE VARIABLE oPersistent AS JPNode NO-UNDO .
    DEFINE VARIABLE cPersistent AS CHARACTER NO-UNDO.
    
    /* Mike Fechner, Consultingwerk Ltd. 24.10.2012
       Ignore trigger revert */
    IF ProparseHelper:HasChildNodeOfNodeType (poASTNode, "REVERT":U) THEN 
        RETURN .
    
    ASSIGN oChild = poASTNode:lastChild () .

    IF NOT VALID-OBJECT (oChild) THEN RETURN . 
    
    ASSIGN oGrandChild = oChild:lastChild () .
    
    IF NodeTypes:getTypeName(oChild:getType()) <> "DO":U OR 
       ProparseHelper:GetSubtypeName (oChild:getSubTypeIndex()) <> "BlockNode":U THEN DO:
        /* RUN TRIGGER ? */
        ASSIGN oChild = poASTNode:firstChild () 
               oGrandChild2 = poASTNode:lastDescendant ().
        
        /* We need to search for the RUN node as it's not always the last node of the 
           subtree */
        DO WHILE VALID-OBJECT (oChild):        
        IF NodeTypes:getTypeName(oChild:getType()) = "RUN":U AND
           ProparseHelper:GetSubtypeName (oChild:getSubTypeIndex()) = "JPNode":U THEN DO:

                ASSIGN oPersistent  = oChild:prevNode () 
                       oGrandChild  = oChild:nextSibling ().
                IF NOT VALID-OBJECT (oGrandChild) THEN 
                    oGrandChild = oChild:firstChild () .

                IF VALID-OBJECT (oPersistent) AND NodeTypes:getTypeName(oPersistent:getType()) = "PERSISTENT":U THEN 
                ASSIGN cPersistent = "PERSISTENT":U .
            ELSE 
                ASSIGN cPersistent = "":U . 

                FIND ttTrigger WHERE ttTrigger.FileName     = poASTNode:getFileName()
                                 AND ttTrigger.LineAnfang   = poASTNode:getLine()
                                 AND ttTrigger.ColumnAnfang = poASTNode:getColumn()
                    NO-ERROR  .
                IF NOT AVAILABLE ttTrigger THEN DO:
                    CREATE ttTrigger . 
                    ASSIGN ttTrigger.FileName     = poASTNode:getFileName()
                           ttTrigger.LineAnfang   = poASTNode:getLine()
                           ttTrigger.ColumnAnfang = poASTNode:getColumn() . 
                END. 
                ASSIGN ttTrigger.EnclosedStartLine   = oChild:getLine ()
                       ttTrigger.EnclosedStartColumn = oChild:getColumn ()
                       ttTrigger.EnclosedEndLine     = oGrandChild2:getLine ()
                       ttTrigger.EnclosedEndColumn   = oGrandChild2:getColumn ()
                       ttTrigger.ProcedureName       = oGrandChild:getText()
                       ttTrigger.EventList           = ProparseHelper:TriggerEventList (poASTNode)
                       ttTrigger.WidgetRef           = ProparseHelper:TriggerWidgetRef (poASTNode)
                       NO-ERROR  .
                IF cPersistent > "":U THEN 
                    ASSIGN ttTrigger.PersistentTrigger = TRUE . 
                RETURN .  
            END.
            oChild = oChild:nextSibling () .            
        END.  

        ASSIGN oChild       = poASTNode:lastChild ()
               oGrandChild2 = oChild:lastDescendant ()  .
    
        FIND ttTrigger WHERE ttTrigger.File         = poASTNode:getFileName()
                         AND ttTrigger.LineAnfang   = poASTNode:getLine()
                         AND ttTrigger.ColumnAnfang = poASTNode:getColumn()
            NO-ERROR  .
        IF NOT AVAILABLE ttTrigger THEN DO:
            CREATE ttTrigger . 
            ASSIGN ttTrigger.File         = poASTNode:getFileName()
                   ttTrigger.LineAnfang   = poASTNode:getLine()
                   ttTrigger.ColumnAnfang = poASTNode:getColumn() . 
        END. 
        /* maybe implicit ASSIGN node */
        IF oChild:getLine () = 0 OR oChild:getColumn () = 0 THEN 
            oChild = Consultingwerk.Studio.Proparse.ProparseHelper:FindFirstChildByPositionInCode (oChild) .
        
        ASSIGN ttTrigger.EnclosedStartLine   = oChild:getLine ()
               ttTrigger.EnclosedStartColumn = oChild:getColumn ()
               ttTrigger.EnclosedEndLine     = oGrandChild2:getLine ()
               ttTrigger.EnclosedEndColumn   = oGrandChild2:getColumn () 
               ttTrigger.EventList           = ProparseHelper:TriggerEventList (poASTNode)
               ttTrigger.WidgetRef           = ProparseHelper:TriggerWidgetRef (poASTNode)
               NO-ERROR .
    END.                    

END PROCEDURE.

PROCEDURE WalkAST:
/*------------------------------------------------------------------------------
    Purpose: Recursively processes the proparse AST for trigger blocks                                                                     
    Notes:                                          
    @param poASTNode The current JPNode                               
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poASTNode AS JPNode NO-UNDO . 

    DO WHILE VALID-OBJECT (poASTNode):

        IF NodeTypes:getTypeName(poASTNode:getType()) = "ON":U AND 
           ProparseHelper:GetSubtypeName (poASTNode:getSubTypeIndex()) = "BlockNode":U THEN 
            
            RUN ProcessTriggerBlock (poASTNode) . 

        IF VALID-OBJECT (poASTNode:firstChild()) THEN 
            RUN WalkAST (poASTNode:firstChild()).
        
        poASTNode = poASTNode:nextSibling () .
    END.

END PROCEDURE.
