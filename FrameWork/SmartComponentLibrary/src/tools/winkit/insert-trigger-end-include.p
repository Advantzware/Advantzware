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
    File        : insert-trigger-end-include.p
    Purpose     : Inserts the triggerend.i Include file into all triggers
                  This program uses proparse to identify trigger blocks 
                  and the compiler XREF to locate include files (not 
                  returned by proparse).

    Syntax      :

    Description : This file is part of the WinKit MTK

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 16 22:50:20 CEST 2011
    Notes       : It is recommendet to ensure that all triggers are refactored
                  to blocks using the procedure refactor-nonblock-triggers.p
                  single statement triggers or ON ... RUN triggers cannot
                  contain additional code. 
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
DEFINE VARIABLE iOffset    AS INTEGER   NO-UNDO.

DEFINE VARIABLE cTriggerPattern AS CHARACTER NO-UNDO INIT "Advantzware/WinKit/winkit-panel-triggerend.i":U.
/*DEFINE VARIABLE cTriggerPattern AS CHARACTER NO-UNDO INIT "triggerend.i":U.*/

DEFINE VARIABLE cFileName       AS CHARACTER NO-UNDO.

{Consultingwerk/Studio/Xref/dsXmlXref.i}
{tools/winkit/ttInclude.i}
{tools/winkit/ttTrigger.i}

&GLOBAL-DEFINE dlm CHR(10)
&GLOBAL-DEFINE timestamp STRING (NOW, "99.99.9999 HH:MM:SS":U) 

/* ***************************  Main Block  *************************** */

OUTPUT STREAM err TO inserttrigger.log UNBUFFERED APPEND .

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
FOR EACH ttTrigger WHERE NOT (ttTrigger.EventList = "CLOSE":U 
                          AND ttTrigger.WidgetRef = "THIS-PROCEDURE":U)
    BREAK BY ttTrigger.FileName:

    IF FIRST-OF (ttTrigger.FileName) THEN DO:
        COPY-LOB FROM FILE ttTrigger.FileName TO lcFile .
        ASSIGN iOffset  = 0
               lChanged = FALSE .
    END.

    FILE-INFO:FILE-NAME = ttTrigger.FileName .
    ASSIGN cFileName = FILE-INFO:FULL-PATHNAME . 

    FIND FIRST ttIncludeFile WHERE ttIncludeFile.SourceFileName  = cFileName
                               AND ttIncludeFile.IncludeFileName = cTriggerPattern
                               AND ttIncludeFile.LineNum         >= ttTrigger.LineAnfang
                               AND ttIncludeFile.LineNum         <= ttTrigger.EnclosedStartLine
        NO-ERROR .

    IF NOT AVAILABLE ttIncludeFile THEN DO:

        IF ttTrigger.LineAnfang = ttTrigger.EnclosedStartLine THEN DO:
            PUT STREAM err UNFORMATTED {&timestamp} " ":U STRING (pcFileName) ": Trigger DO and END in a single line is not expected! ":U
                                       "unable to refactor ":U ttTrigger.FileName " at ":U ttTrigger.EnclosedStartLine SKIP (1).
            NEXT triggerLoop.
        END.

        lcFile = Consultingwerk.Util.ListHelper:InsertEntry (lcFile, ttTrigger.EnclosedStartLine + iOffset, "":U, {&dlm}) .
        lcFile = Consultingwerk.Util.ListHelper:InsertEntry (lcFile, ttTrigger.EnclosedStartLine + iOffset + 1, SUBSTITUTE ("  ~{ &1 ~}":U, cTriggerPattern), {&dlm}) .

        ASSIGN lChanged = TRUE
               iOffset = iOffset + 2 .
    END.

    IF LAST-OF (ttTrigger.FileName) AND lChanged THEN
        COPY-LOB FROM lcFile TO FILE ttTrigger.FileName .
END. 

PROCEDURE ProcessTriggerBlock:
/*------------------------------------------------------------------------------
    Purpose: Processes a TriggerBlock identified by WalkAST and checks if it                                      
    Notes:   Contains the triggerend include file                                      
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poASTNode AS JPNode NO-UNDO . 

    DEFINE VARIABLE oEvents      AS JPNode    NO-UNDO .
    DEFINE VARIABLE oChild       AS JPNode    NO-UNDO .
    DEFINE VARIABLE oGrandChild  AS JPNode    NO-UNDO .
    DEFINE VARIABLE oGrandChild2 AS JPNode    NO-UNDO .

    /* Mike Fechner, Consultingwerk Ltd. 24.10.2012
       Ignore trigger revert */
    IF ProparseHelper:HasChildNodeOfNodeType (poASTNode, "REVERT":U) THEN 
        RETURN .

    /* Mike Fechner, Consultingwerk Ltd. 01.09.2011
       Skip triggers that are not for UI events (database triggers) */
    ASSIGN oEvents = poASTNode:firstChild () .
    
    IF NOT VALID-OBJECT (oEvents) OR NodeTypes:getTypeName(oEvents:getType()) <> "Event_list":U THEN RETURN .
    
    ASSIGN oChild = poASTNode:lastChild () .

    IF NOT VALID-OBJECT (oChild) OR NodeTypes:getTypeName(oChild:getType()) <> "DO":U THEN RETURN . 
    
    ASSIGN oGrandChild = oChild:lastChild () .
    
    IF NOT VALID-OBJECT (oGrandChild) THEN RETURN . 
    
    ASSIGN oGrandChild2 = oGrandChild:prevNode () . 
    
    IF NOT VALID-OBJECT (oGrandChild2) OR NodeTypes:getTypeName(oGrandChild2:getType()) <> "END":U THEN RETURN . 
    
    FIND ttTrigger WHERE ttTrigger.FileName     = oChild:getFileName ()
                     AND ttTrigger.LineAnfang   = oChild:getLine ()
                     AND ttTrigger.ColumnAnfang = oChild:getColumn () NO-ERROR .
                     
    IF AVAILABLE ttTrigger THEN
        RETURN .
        
    CREATE ttTrigger. 
    ASSIGN ttTrigger.FileName            = oChild:getFileName ()
           ttTrigger.LineAnfang          = oChild:getLine ()
           ttTrigger.ColumnAnfang        = oChild:getColumn ()
           ttTrigger.EnclosedStartLine   = oGrandChild2:getLine () 
           ttTrigger.EnclosedStartColumn = oGrandChild2:getColumn ()
           ttTrigger.EventList           = ProparseHelper:TriggerEventList (poASTNode)
           ttTrigger.WidgetRef           = ProparseHelper:TriggerWidgetRef (poASTNode)
           .
    
END PROCEDURE.

PROCEDURE WalkAST:
/*------------------------------------------------------------------------------
    Purpose: Recursively processes the AST of the files                                                                      
    Notes:                                                                        
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
