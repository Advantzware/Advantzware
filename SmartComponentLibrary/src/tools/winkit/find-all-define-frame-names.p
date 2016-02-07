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
    File        : find-all-define-frame-names.p
    Purpose     : An utility to find all frames defined using a DEFINE FRAME 
                  statement

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 24 10:26:22 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Studio.Proparse.* FROM PROPATH .
USING org.prorefactor.refactor.* FROM ASSEMBLY.
USING org.prorefactor.treeparser.* FROM ASSEMBLY.
USING org.prorefactor.core.* FROM ASSEMBLY.
USING com.joanju.proparse.NodeTypes FROM ASSEMBLY .

DEFINE INPUT  PARAMETER pcFileName      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER plRecurseBlocks AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER pcLogFile       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcFrameNames    AS CHARACTER NO-UNDO.

DEFINE VARIABLE oParseUnit AS ParseUnit NO-UNDO . 

DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE STREAM err .
 
{tools/winkit/ttTrigger.i}

DEFAULT-WINDOW:WIDTH = 320  .
DEFAULT-WINDOW:HEIGHT = 30  .
DEFAULT-WINDOW:TITLE = pcFileName .

ProparseHelper:SetProparseEnvironment() . 
ProparseHelper:ExportDatabaseSchema () .

OUTPUT STREAM err TO VALUE (pcLogFile) UNBUFFERED APPEND .

ProparseHelper:Initialize() .  

DISPL i pcFileName FORMAT "x(100)":U WITH DOWN WIDTH 300.  

/*    PUT STREAM err UNFORMATTED "Processing (":U STRING(i) "): ":U STRING (cFile) SKIP.*/
DOWN .
PROCESS EVENTS . 

DO ON ERROR UNDO, THROW:
    oParseUnit = Consultingwerk.Studio.Proparse.ProparseHelper:ParseFile (pcFileName) .

    CATCH err AS Progress.Lang.Error:
      
        PUT STREAM err UNFORMATTED STRING (pcFileName) ":":U TRIM (SUBSTRING (err:GetMessage(1), 1, 2000)) SKIP (1). 
        NEXT .
    END CATCH.
END.

RUN WalkAST (oParseUnit:getTopNode():firstchild()) .

ASSIGN pcFrameNames = TRIM (pcFrameNames, ",":U) .

CATCH err AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
END CATCH.

/* **********************  Internal Procedures  *********************** */

PROCEDURE ProcessFrame:
/*------------------------------------------------------------------------------
    Purpose: Processes the JPNode that represents a Trigger and checks if it contains
             a block, a single statement or a RUN (PERSISTENT) statement.                                                                     
    Notes:      
    @param poASTNode The current JPNode                               
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poASTNode AS JPNode NO-UNDO . 

    DEFINE VARIABLE oIDNode    AS JPNode NO-UNDO . 
    DEFINE VARIABLE cFrameName AS CHARACTER NO-UNDO.
    
    oIDNode = ProparseHelper:FindChildNodeOfNodeType (poASTNode, "ID":U) .

    IF VALID-OBJECT (oIDNode) THEN DO:
        ASSIGN cFrameName = oIDNode:getText () . 
    
        IF LOOKUP (cFrameName, pcFrameNames) = 0 THEN 
            ASSIGN pcFrameNames = pcFrameNames + ",":U + cFrameName .
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

        IF NodeTypes:getTypeName(poASTNode:getType()) = "PROCEDURE":U THEN DO:
            poASTNode = poASTNode:nextSibling () .
            NEXT . 
        END.

/*IF NodeTypes:getTypeName(poASTNode:getType()) = "DEFINE":U THEN                             */
/*MESSAGE "define" VALID-OBJECT (ProparseHelper:FindChildNodeOfNodeType (poASTNode, "FRAME":U)*/
/*    VIEW-AS ALERT-BOX.                                                                      */

        IF NodeTypes:getTypeName(poASTNode:getType()) = "DEFINE":U AND 
           VALID-OBJECT (ProparseHelper:FindChildNodeOfNodeType (poASTNode, "FRAME":U)) THEN 
            
            RUN ProcessFrame (poASTNode) . 

        IF plRecurseBlocks AND VALID-OBJECT (poASTNode:firstChild()) THEN 
            RUN WalkAST (poASTNode:firstChild()).
        
        poASTNode = poASTNode:nextSibling () .
    END.

END PROCEDURE.
