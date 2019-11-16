/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : AssignSkipDictionaryParser
    Purpose     : Parses the invokations to the AssignSkipDictionary method
                  in the constructor of a DataAccess class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 01 20:03:58 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.Studio.Proparse.NodeTypes.* FROM PROPATH.
USING com.joanju.proparse.NodeTypes                    FROM ASSEMBLY .
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING Consultingwerk.Studio.Proparse.*                 FROM PROPATH .
USING org.prorefactor.core.JPNode                      FROM ASSEMBLY .
USING org.prorefactor.nodetypes.*                      FROM ASSEMBLY .
USING org.prorefactor.treeparser.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .


CLASS Consultingwerk.BusinessEntityDesigner.Services.AssignSkipDictionaryParser:

    /**
     * Purpose: Parses the Parse Unit for the first AssignSkipDictionary invokations
     *          in the first contructor with a single parameter
     * Notes:
     * @param poParseUnit The proparse ParseUnit
     * @return The ListAssignSkipDictionaryEntry with the parse results
     */
    METHOD PUBLIC ListAssignSkipDictionaryEntry Parse (poParseUnit AS ParseUnit):

        DEFINE VARIABLE oCodeblock   AS JPNode                        NO-UNDO .
        DEFINE VARIABLE oWrapper     AS JPNodesWrapper                NO-UNDO .
        DEFINE VARIABLE oResult      AS ListAssignSkipDictionaryEntry NO-UNDO .

        oResult = NEW ListAssignSkipDictionaryEntry () .

        oCodeblock = ProparseHelper:GetChildNodeByPath (poParseUnit:getTopNode(), "CLASS~\Code_block":U) .

        oWrapper = NEW JPNodesWrapper (oCodeblock) .
        oWrapper:NodeTypeFilter = "CONSTRUCTOR":U .

        {Consultingwerk/foreachABL.i JPNode oConstructor IN oWrapper}

            IF ProparseHelper:ParseParameterList(oConstructor) = "INPUT HANDLE":U THEN DO:

                oCodeblock = ProparseHelper:FindChildNodeOfNodeType(oConstructor, "Code_block":U) .

                THIS-OBJECT:RecurseNodes (oCodeblock, oResult) .

            END.
        END.

        RETURN oResult .

    END METHOD.

    /**
     * Purpose: Parses an AssignSkipDictionary statement and adds entries
     *          to the result list
     * Notes:
     * @param poStatement The statement node
     * @param poResult The list of result entries
     */
    METHOD PROTECTED VOID ParseStatement (poStatement AS JPNode,
                                          poResult AS ListAssignSkipDictionaryEntry):

        DEFINE VARIABLE oParameter AS JPNode NO-UNDO .

        DEFINE VARIABLE cBufferName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFields     AS CHARACTER NO-UNDO.

        oParameter = ProparseHelper:GetChildNodeByPath (poStatement, "Widget_ref~\Method_param_list":U) .

        ASSIGN cBufferName = StringHelper:Unquote (ProparseHelper:GetChildNodeByPath (oParameter, "2~\QSTRING":U):getText())
               cFields     = StringHelper:Unquote (ProparseHelper:GetChildNodeByPath (oParameter, "4~\QSTRING":U):getText()) .

        poResult:Add (cBufferName, cFields, poStatement) .

    END METHOD.

    /**
     * Purpose: Parses the given node for AssignSkipDictionary calls
     * Notes:   Recursive method
     * @param poParentNode The parent node to iterate
     * @param poResult The list to add results to
     */
    METHOD PROTECTED VOID RecurseNodes (poParentNode AS JPNode,
                                        poResult     AS ListAssignSkipDictionaryEntry):

        DEFINE VARIABLE oWrapper     AS JPNodesWrapper   NO-UNDO .
        DEFINE VARIABLE oId          AS JPNode           NO-UNDO .
        DEFINE VARIABLE oNode        AS JPNode           NO-UNDO .

        oWrapper = NEW JPNodesWrapper (poParentNode) .
        oWrapper:NodeTypeFilter = "Expr_statement":U .

        {Consultingwerk/foreachABL.i JPNode oStatement IN oWrapper}

            oId = ProparseHelper:GetChildNodeByPath (oStatement, "Widget_Ref~\Field_Ref~\ID":U) .

            IF VALID-OBJECT (oId) AND oId:getText() = "AssignSkipDictionary":U THEN

                THIS-OBJECT:ParseStatement (oStatement, poResult) .

            ELSE DO:

                oId = ProparseHelper:GetChildNodeByPath (oStatement, "Widget_Ref~\ID":U) .

                IF VALID-OBJECT (oId) AND oId:getText() = "AssignSkipDictionary":U THEN

                    THIS-OBJECT:ParseStatement (oStatement, poResult) .
            END.
        END .

        /* Recursion */
        oNode = poParentNode:firstChild () .

        DO WHILE VALID-OBJECT (oNode):
            IF VALID-OBJECT (oNode:firstChild()) THEN
                THIS-OBJECT:RecurseNodes(oNode, poResult).

            oNode = oNode:nextSibling () .
        END.

    END METHOD.

END CLASS.
