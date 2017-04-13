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
    File        : insert-embedfinalize-include.p
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

DEFINE VARIABLE cTriggerPattern AS CHARACTER NO-UNDO INIT "src/winkit/embedfinalize.i":U.
DEFINE VARIABLE cFileName       AS CHARACTER NO-UNDO.

/* Filter by Event and Widget_ref */
DEFINE VARIABLE cWaitForEvent     AS CHARACTER NO-UNDO INIT "CLOSE":U.
DEFINE VARIABLE cWaitForWidgetRef AS CHARACTER NO-UNDO INIT "THISPROCEDURE":U /* this needs to match a proparse node type */.

/*DEFINE VARIABLE cTriggerPattern AS CHARACTER NO-UNDO INIT "closewindow.i":U.*/

{Consultingwerk/Studio/Xref/dsXmlXref.i}
{tools/winkit/ttInclude.i}
{tools/winkit/ttTrigger.i}

&GLOBAL-DEFINE dlm CHR(10)
&GLOBAL-DEFINE timestamp STRING (NOW, "99.99.9999 HH:MM:SS":U) 

/* ***************************  Main Block  *************************** */

OUTPUT STREAM err TO insert-embedfinalize.log UNBUFFERED APPEND .

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
FOR EACH ttTrigger 
    BREAK BY ttTrigger.FileName
          BY ttTrigger.LineAnfang DESCENDING:

    IF FIRST-OF (ttTrigger.FileName) THEN DO:
        COPY-LOB FROM FILE ttTrigger.FileName TO lcFile .
        ASSIGN lChanged = FALSE .
    END.

    FILE-INFO:FILE-NAME = ttTrigger.FileName .
    ASSIGN cFileName = FILE-INFO:FULL-PATHNAME . 

    /* Search for a matching include file one or two lines before the WAIT-FOR block*/
    FIND FIRST ttIncludeFile WHERE ttIncludeFile.SourceFileName  = cFileName
                               AND ttIncludeFile.IncludeFileName = cTriggerPattern
                               AND ttIncludeFile.LineNum         >= ttTrigger.LineAnfang - 3
                               AND ttIncludeFile.LineNum         <= ttTrigger.LineAnfang
        NO-ERROR .

    IF NOT AVAILABLE ttIncludeFile THEN DO:

        lcFile = Consultingwerk.Util.ListHelper:InsertEntry 
                        (lcFile, 
                         ttTrigger.LineAnfang, 
                         SUBSTITUTE ("  ~{ &1 ~}":U, cTriggerPattern), 
                         {&dlm}) .

        lcFile = Consultingwerk.Util.ListHelper:InsertEntry 
                        (lcFile, 
                         ttTrigger.LineAnfang, 
                         "":U, 
                         {&dlm}) .
                         
        lcFile = Consultingwerk.Util.ListHelper:InsertEntry 
                        (lcFile, 
                         ttTrigger.LineAnfang + 2, 
                         "":U, 
                         {&dlm}) .
     
        ASSIGN lChanged = TRUE . 
    END.

    IF LAST-OF (ttTrigger.FileName) AND lChanged THEN
        COPY-LOB FROM lcFile TO FILE ttTrigger.FileName .
END.

PROCEDURE ProcessWaitForStatement:
/*------------------------------------------------------------------------------
    Purpose: Processes a WAIT-FOR statement identified by WalkAST and creates 
             ttTrigger blocks for the contained RUN statemetns                                      
    Notes:   
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poASTNode AS JPNode NO-UNDO . 

    DEFINE VARIABLE oParent AS JPNode    NO-UNDO .
    DEFINE VARIABLE oChild  AS JPNode    NO-UNDO . 
    DEFINE VARIABLE oChild2 AS JPNode    NO-UNDO . 

    DEFINE VARIABLE cEvent  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWidget AS CHARACTER NO-UNDO.

    /* if procedure has already been processed, skip it 
       (typically the case for WAIT-FOR statement in an include file 
        that is referenced twice) */
    FIND ttTrigger WHERE ttTrigger.FileName     = poASTNode:getFileName ()
                     AND ttTrigger.LineAnfang   = poASTNode:getLine ()
                     AND ttTrigger.ColumnAnfang = poASTNode:getColumn () NO-ERROR .
                  
    IF AVAILABLE ttTrigger THEN
        RETURN .

    oChild = Consultingwerk.Studio.Proparse.ProparseHelper:FindChildNodeOfNodeType (poASTNode,
                                                                                    "Event_list":U) .

    IF VALID-OBJECT (oChild) THEN DO:
        oChild2 = oChild:firstChild () .
        
        IF VALID-OBJECT (oChild2) THEN 
            ASSIGN cEvent = NodeTypes:getTypeName(oChild2:getType()) .
    END.

    oChild = Consultingwerk.Studio.Proparse.ProparseHelper:FindChildNodeOfNodeType (poASTNode,
                                                                                    "Widget_ref":U) .

    IF VALID-OBJECT (oChild) THEN DO:
        oChild2 = oChild:firstChild () .
        
        IF VALID-OBJECT (oChild2) THEN 
            ASSIGN cWidget = NodeTypes:getTypeName(oChild2:getType()) .
    END.

    IF cWaitForEvent > "":U AND cWaitForWidgetRef > "":U THEN 
        IF cEvent  <> cWaitForEvent OR 
           cWidget <> cWaitForWidgetRef THEN 
           RETURN . 

    oParent = poASTNode:parent ().
    
    /* If the WAIT-FOR statement is child of an IF statement, it's a 
       IF NOT THIS-PROCEDURE:PERSISTENT THEN WAIT-FOR ... */
    IF NodeTypes:getTypeName(oParent:getType()) = "IF":U AND 
       ProparseHelper:GetSubtypeName (oParent:getSubTypeIndex()) = "JPNode":U THEN 
        poASTNode = oParent .   

    CREATE ttTrigger.
    ASSIGN ttTrigger.FileName               = poASTNode:getFileName ()
           ttTrigger.LineAnfang             = poASTNode:getLine ()
           ttTrigger.ColumnAnfang           = poASTNode:getColumn () .
           
END PROCEDURE.

PROCEDURE WalkAST:
/*------------------------------------------------------------------------------
    Purpose: Recursively processes the AST of the files                                                                      
    Notes:                                                                        
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poASTNode AS JPNode NO-UNDO . 

    DO WHILE VALID-OBJECT (poASTNode):

        IF NodeTypes:getTypeName(poASTNode:getType()) = "WAITFOR":U AND 
           ProparseHelper:GetSubtypeName (poASTNode:getSubTypeIndex()) = "JPNode":U THEN 
            
            RUN ProcessWaitForStatement (poASTNode) . 

        IF VALID-OBJECT (poASTNode:firstChild()) THEN 
            RUN WalkAST (poASTNode:firstChild()).
        
        poASTNode = poASTNode:nextSibling () .
    END.

END PROCEDURE.
