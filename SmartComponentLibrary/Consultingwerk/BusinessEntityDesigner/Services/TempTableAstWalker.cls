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
    File        : TempTableAstWalker
    Purpose     : Walks the Proparse AST searching for Temp-Table nodes
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 20 06:54:00 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING com.joanju.proparse.NodeTypes                    FROM ASSEMBLY .
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH . 
USING Consultingwerk.Framework.Collections.*           FROM PROPATH . 
USING Consultingwerk.Studio.Proparse.*                 FROM PROPATH . 
USING org.prorefactor.core.JPNode                      FROM ASSEMBLY .
USING org.prorefactor.treeparser.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.TempTableAstWalker: 

    /*------------------------------------------------------------------------------
        Purpose: Raised when a Temp-Table Node is found in the AST
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The TempTableAstNodeAvailableEventArgs with the data for this event                                                                     
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT TempTableAstNodeAvailable SIGNATURE VOID 
        (sender AS Progress.Lang.Object, e AS TempTableAstNodeAvailableEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TempTableAstWalker class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TempTableAstWalker ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Raises the TempTableAstNodeAvailable event
        Notes:   
        @param e The TempTableAstNodeAvailableEventArgs with the Data for this Event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnTempTableAstNodeAvailable (e AS TempTableAstNodeAvailableEventArgs):
        
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "TempTableAstNodeAvailable":U) .

        THIS-OBJECT:TempTableAstNodeAvailable:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Processes a parse unit
        Notes:   
        @param poParseUnit The ParseUnit to Process
        @param poController The reference to the BusinessEntityDatasetController
        @param poTables A CharacterList of TempTables
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ProcessAst (poParseUnit AS ParseUnit,
                                   poController AS BusinessEntityDatasetController,
                                   poTables AS CharacterList):
        
        THIS-OBJECT:WalkAst (poParseUnit:getTopNode():firstchild(), 
                             poController,
                             poTables) . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Walks the AST and searches for Temp-Tables
        Notes:   
        @param poASTNode The JPNode to iterate
        @param poController The reference to the BusinessEntityDatasetController instance
        @param poList The List to add Temp-Table names to
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID WalkAst (poASTNode AS JPNode,
                                   poController AS BusinessEntityDatasetController,
                                   poList AS CharacterList):
        
        DEFINE VARIABLE oNode   AS JPNode NO-UNDO . 
        DEFINE VARIABLE oIDNode AS JPNode NO-UNDO . 
        
        ASSIGN oNode = poASTNode . 

        DO WHILE VALID-OBJECT (oNode):

            CASE NodeTypes:getTypeName (oNode:getType()):
                WHEN "DEFINE":U THEN DO:
                    IF VALID-OBJECT (ProparseHelper:FindChildNodeOfNodeType (oNode, "TEMPTABLE":U)) THEN DO:
                        
                        oIDNode = ProparseHelper:FindChildNodeOfNodeType (oNode, "ID":U) .
                         
                        IF VALID-OBJECT (oIDNode) THEN 
                            OnTempTableAstNodeAvailable (NEW TempTableAstNodeAvailableEventArgs (oIDNode:getText (),
                                                                                                 oNode,
                                                                                                 poController,
                                                                                                 poList)) . 
                    END.
                END.
                WHEN "CLASS":U OR WHEN "Code_block":U THEN DO:
                    /* Allow recursive call for CLASS block */
                    
                    WalkAST (oNode:firstChild (), poController, poList) .
                END.
        
            END CASE.
                    
            oNode = oNode:nextSibling () .
        END.    
        
    END METHOD .

END CLASS.
