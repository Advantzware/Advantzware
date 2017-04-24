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
    File        : insert-closewindow-include.p
    Purpose     : Inserts the closewindow.i include file just before or 
                  after the RUN disable_UI call, if not present in the same code 
                  block

    Syntax      :

    Description : This file is part of the WinKit MTK

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 18 07:31:54 CEST 2012
    Notes       : This program should only be called after all 
                  ON CLOSE OF THIS-PROCEDURE Triggers have been turned into
                  blocks (Trigger including DO: END.), i.e. by running 
                  find-nonblock-trigger.p and refactor-nonblock-trigger.p
                  or search and replace
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Studio.Proparse.* FROM PROPATH .
USING org.prorefactor.refactor.* FROM ASSEMBLY.
USING org.prorefactor.treeparser.* FROM ASSEMBLY.
USING org.prorefactor.core.* FROM ASSEMBLY.
USING com.joanju.proparse.NodeTypes FROM ASSEMBLY .

DEFINE STREAM err . 

DEFINE INPUT  PARAMETER pcFileName AS CHARACTER NO-UNDO.

DEFINE VARIABLE oParseUnit AS ParseUnit NO-UNDO . 
DEFINE VARIABLE lcFile     AS LONGCHAR  NO-UNDO .
DEFINE VARIABLE lChanged   AS LOGICAL NO-UNDO.

DEFINE VARIABLE cTriggerPattern AS CHARACTER NO-UNDO INIT "src/winkit/closewindow.i":U.
/*DEFINE VARIABLE cTriggerPattern AS CHARACTER NO-UNDO INIT "closewindow.i":U.*/
DEFINE VARIABLE lBeforeRun AS LOGICAL NO-UNDO INIT TRUE.    /* insert before RUN disable_UI */
DEFINE VARIABLE cFileName  AS CHARACTER NO-UNDO.

{Consultingwerk/Studio/Xref/dsXmlXref.i}
{tools/winkit/ttInclude.i}
{tools/winkit/ttTrigger.i}

&GLOBAL-DEFINE dlm CHR(10)
&GLOBAL-DEFINE timestamp STRING (NOW, "99.99.9999 HH:MM:SS":U) 

/* ***************************  Main Block  *************************** */

OUTPUT STREAM err TO insertclosewindow.log UNBUFFERED APPEND .

ProparseHelper:SetProparseEnvironment() . 
ProparseHelper:ExportDatabaseSchema () .
ProparseHelper:Initialize() . 

DO ON ERROR UNDO, THROW:

    Consultingwerk.Studio.CompilerHelper:GenerateXmlXref (pcFileName, 
                                                          OUTPUT DATASET dsXmlXref) .

    CATCH err AS Progress.Lang.Error:

        PUT STREAM err UNFORMATTED {&timestamp} " ":U STRING (pcFileName) ": Error during compilation : ":U err:GetMessage (1) SKIP (1). 
        RETURN .
    		
    END CATCH.
END.

DO ON ERROR UNDO, THROW:
    oParseUnit = Consultingwerk.Studio.Proparse.ProparseHelper:ParseFile (pcFileName) .

    CATCH err AS Progress.Lang.Error:
        PUT STREAM err UNFORMATTED {&timestamp} " ":U STRING (pcFileName) ":":U TRIM (SUBSTRING (err:GetMessage(1), 1, 2000)) SKIP (1). 
        NEXT .
        
    END CATCH.
END.

RUN WalkAST (oParseUnit:getTopNode():firstchild()) .

/* Creates a single temp-table containing all references to include files
   in a single temp-table (rather than eReference and eSource) */
FOR EACH eReference WHERE eReference.ReferenceType = "INCLUDE":U:
    FIND eSource WHERE eSource.SourceGuid = eReference.SourceGuid 
                   AND eSource.FileNum    = eReference.FileNum.

    FIND ttIncludeFile WHERE ttIncludeFile.SourceFileName  = eSource.FileName
                         AND ttIncludeFile.IncludeFileName = ENTRY (1, TRIM(eReference.ObjectIdentifier), " ":U)
                         AND ttIncludeFile.LineNum         = eReference.LineNum 
                         
                         NO-ERROR .  
    
    IF NOT AVAILABLE ttIncludeFile THEN DO:
        CREATE ttIncludeFile . 
        ASSIGN ttIncludeFile.SourceFileName  = eSource.FileName
               ttIncludeFile.IncludeFileName = ENTRY (1, TRIM(eReference.ObjectIdentifier), " ":U)
               ttIncludeFile.LineNum         = eReference.LineNum .
    END.
END.

/* Skip ON CLOSE OF THIS-PROCEDURE Trigger when inserting the triggerend include file */
triggerLoop:
FOR EACH ttTrigger WHERE ttTrigger.ProcedureName = "disable_UI":U
                     AND ttTrigger.SurroundingStartLine > 0 
    BREAK BY ttTrigger.FileName
          BY ttTrigger.LineAnfang DESCENDING:

    IF FIRST-OF (ttTrigger.FileName) THEN DO:
        COPY-LOB FROM FILE ttTrigger.FileName TO lcFile .
        ASSIGN lChanged = FALSE .
    END.

    FILE-INFO:FILE-NAME = ttTrigger.FileName .
    ASSIGN cFileName = FILE-INFO:FULL-PATHNAME . 

    /* Search for a matching include file between the surrounding begin and end lines */
    FIND FIRST ttIncludeFile WHERE ttIncludeFile.SourceFileName  = cFileName
                               AND ttIncludeFile.IncludeFileName = cTriggerPattern
                               AND ttIncludeFile.LineNum         >= ttTrigger.SurroundingStartLine
                               AND ttIncludeFile.LineNum         <= ttTrigger.SurroundingEndLine
        NO-ERROR .

    IF NOT AVAILABLE ttIncludeFile THEN DO:

        IF ttTrigger.SurroundingStartLine = ttTrigger.SurroundingEndLine THEN DO:
            PUT STREAM err UNFORMATTED {&timestamp} " ":U STRING (pcFileName) ": Trigger DO and END in a single line is not expected! ":U
                                       "unable to refactor ":U ttTrigger.ProcedureName " at ":U ttTrigger.EnclosedStartLine ttTrigger.EnclosedStartColumn SKIP (1).
            NEXT triggerLoop.
        END.

        lcFile = Consultingwerk.Util.ListHelper:InsertEntry 
                        (lcFile, 
                         ttTrigger.LineAnfang + (IF lBeforeRun THEN 0 ELSE 1), 
                         SUBSTITUTE ("  ~{ &1 ~}":U, cTriggerPattern), {&dlm}) .
    
        ASSIGN lChanged = TRUE . 
    END.

    IF LAST-OF (ttTrigger.FileName) AND lChanged THEN
        COPY-LOB FROM lcFile TO FILE ttTrigger.FileName .
END.

PROCEDURE ProcessRunBlock:
/*------------------------------------------------------------------------------
    Purpose: Processes a RUN identified by WalkAST and creates ttTrigger blocks
             for the contained RUN statemetns                                      
    Notes:   
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poASTNode AS JPNode NO-UNDO . 

    DEFINE VARIABLE oChild       AS JPNode    NO-UNDO .
    DEFINE VARIABLE oLastChild   AS JPNode    NO-UNDO .
    DEFINE VARIABLE oParent      AS JPNode    NO-UNDO . 

    /* if procedure has already been processed, skip it 
       (typically the case for RUN statement in an include file 
        that is referenced twice) */
    FIND ttTrigger WHERE ttTrigger.FileName     = poASTNode:getFileName ()
                     AND ttTrigger.LineAnfang   = poASTNode:getLine ()
                     AND ttTrigger.ColumnAnfang = poASTNode:getColumn () NO-ERROR .
                  
    IF AVAILABLE ttTrigger THEN
        RETURN .

    oChild = ProparseHelper:FindChildNodeOfNodeType (poASTNode,
                                                     "FILENAME":U) .
                 
    /* no point in proceeding further if there is no FILENAME node */                                                    
    IF NOT VALID-OBJECT (oChild) THEN 
        RETURN .                                                 

    oParent = poASTNode:parent ().
    
    /* A code block does not have a position of it's own, so use the 
       grand parent */
    IF NodeTypes:getTypeName(oParent:getType()) = "Code_block":U THEN 
        oParent = oParent:parent () .

    CREATE ttTrigger. 
    ASSIGN ttTrigger.FileName               = poASTNode:getFileName ()
           ttTrigger.LineAnfang             = poASTNode:getLine ()
           ttTrigger.ColumnAnfang           = poASTNode:getColumn ()
           ttTrigger.ProcedureName          = oChild:getText () .
           
           
    /* for parent of the .p we can not return the Surrounding positions */
    IF NodeTypes:getTypeName(oParent:getType()) <> "Program_root":U THEN DO:     
        
        oLastChild = JPNode:getLastDescendant (oParent) .
        
        ASSIGN ttTrigger.SurroundingStartLine   = oParent:getLine ()
               ttTrigger.SurroundingStartColumn = oParent:getColumn () 
               ttTrigger.SurroundingEndLine     = oLastChild:getLine ()
               ttTrigger.SurroundingEndColumn   = oLastChild:getColumn ()        
           .
    END.
    
END PROCEDURE.

PROCEDURE WalkAST:
/*------------------------------------------------------------------------------
    Purpose: Recursively processes the AST of the files                                                                      
    Notes:                                                                        
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poASTNode AS JPNode NO-UNDO . 

    DO WHILE VALID-OBJECT (poASTNode):

        IF NodeTypes:getTypeName(poASTNode:getType()) = "RUN":U AND 
           ProparseHelper:GetSubtypeName (poASTNode:getSubTypeIndex()) = "JPNode":U THEN 
            
            RUN ProcessRunBlock (poASTNode) . 

        IF VALID-OBJECT (poASTNode:firstChild()) THEN 
            RUN WalkAST (poASTNode:firstChild()).
        
        poASTNode = poASTNode:nextSibling () .
    END.

END PROCEDURE.
