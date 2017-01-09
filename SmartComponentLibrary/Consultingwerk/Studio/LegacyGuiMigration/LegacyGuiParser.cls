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
    File        : LegacyGuiParser
    Purpose     : Abstract base class for Legacy GUI parser classes
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Mar 07 22:07:23 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                           FROM PROPATH .
USING Consultingwerk.Assertion.*                 FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.* FROM PROPATH .
USING Consultingwerk.Studio.Proparse.*           FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING org.prorefactor.refactor.*                 FROM ASSEMBLY.
USING org.prorefactor.treeparser.*               FROM ASSEMBLY.
USING org.prorefactor.core.*                     FROM ASSEMBLY.
USING com.joanju.proparse.NodeTypes              FROM ASSEMBLY .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Studio.LegacyGuiMigration.LegacyGuiParser ABSTRACT:

    DEFINE PRIVATE   VARIABLE lInitialized      AS LOGICAL   NO-UNDO INIT FALSE .
    DEFINE PROTECTED VARIABLE cNestingNodeTypes AS CHARACTER NO-UNDO
        INIT "BLOCK_LABEL,Code_block":U .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the status of the class has changed
        Notes:
        @param sender The object that raised the StatusChanged event
        @param e The StatusChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT StatusChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                      e AS StatusChangedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Returns a comma delimited list of buffer names from the DEFINE
                 QUERY Statement for the given Query Name
        Notes:
        @param poParseUnit The ParseUnit for the current source
        @param pcQueryName The name of the query to return the buffer names for
        @return The comma delimited list of query buffer names
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetQueryBufferNames (poParseUnit AS ParseUnit,
                                                    pcQueryName AS CHARACTER):

        ObjectAssert:IsValid (poParseUnit, "poParseUnit":U) .
        Assert:NotNullOrEmpty (pcQueryName, "pcQueryName":U) .

        RETURN THIS-OBJECT:GetQueryBufferNames (poParseUnit:getTopNode (),
                                                pcQueryName) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a comma delimited list of buffer names from the DEFINE
                 QUERY Statement for the given Query Name
        Notes:
        @param poNode The current JPNode to process
        @param pcQueryName The name of the query to return the buffer names for
        @return The comma delimited list of query buffer names
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER GetQueryBufferNames (poNode AS JPNode,
                                                  pcQueryName AS CHARACTER):


        DEFINE VARIABLE oChild       AS JPNode         NO-UNDO .
        DEFINE VARIABLE cBufferNames AS CHARACTER      NO-UNDO .
        DEFINE VARIABLE cId          AS CHARACTER      NO-UNDO .
        DEFINE VARIABLE oNodeWrapper AS JPNodesWrapper NO-UNDO .

        oChild = poNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName (oChild:getType()) = "DEFINE":U AND
               NodeTypes:getTypeName (oChild:firstChild():getType()) = "QUERY":U THEN DO:

                cId = ProparseHelper:FindChildNodeOfNodeType (oChild, "ID":U):getText () .

                IF cId <> pcQueryName THEN DO:
                    oChild = oChild:nextSibling () .
                    NEXT .
                END.

                oNodeWrapper = NEW JPNodesWrapper (oChild) .
                oNodeWrapper:NodeTypeFilter = "RECORD_NAME":U  .

                {Consultingwerk/foreachABL.i JPNode oNode in oNodeWrapper}

                    ASSIGN cBufferNames = cBufferNames + oNode:getText() + ",":U .

                END.

                RETURN TRIM (cBufferNames, ",":U) .
            END .

            ELSE IF VALID-OBJECT (oChild:firstChild ()) THEN

                THIS-OBJECT:GetQueryBufferNames (oChild,
                                                 pcQueryName) .

            oChild = oChild:nextSibling () .
        END .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Proparse environment
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Initialize ():

        IF lInitialized THEN
            RETURN .

        ASSIGN lInitialized = TRUE .

        THIS-OBJECT:PublishStatus ("Initializing Proparse environment.":U) .

        ProparseHelper:SetProparseEnvironment() .

        THIS-OBJECT:PublishStatus ("Processing database schema.":U) .

        ProparseHelper:ExportDatabaseSchema () .

        THIS-OBJECT:PublishStatus ("Initializing Proparse.":U) .

        ProparseHelper:Initialize() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the StatusChanged
        Notes:
        @param e The StatusChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnStatusChanged (e AS StatusChangedEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "StatusChanged":U) .

        THIS-OBJECT:StatusChanged:Publish (THIS-OBJECT, e) .

    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Parses the LIST-ITEM-PAIRS Node and returns a CHR(1) delimited list
                 of the LIST-ITEM-PAIRS
        Notes:
        @param poViewAsNode The JPNode representing the VIEW-AS Option
        @return The CHR(1) delimited list of LIST-ITEM-PAIRS
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ParseListItemPairs (poViewAsNode AS JPNode):

        DEFINE VARIABLE cListItemPairs     AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE iListItemPairs     AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE oListItemPairsNode AS JPNode          NO-UNDO .

        ASSIGN cListItemPairs = "":U .

        ASSIGN oListItemPairsNode = ProparseHelper:FindChildNodeOfNodeType (poViewAsNode, "LISTITEMPAIRS":U):firstChild () .

        DO WHILE VALID-OBJECT (oListItemPairsNode):

            IF NodeTypes:getTypeName (oListItemPairsNode:getType()) = "COMMA":U THEN
                cListItemPairs = cListItemPairs + CHR (1) .
            ELSE
                cListItemPairs = cListItemPairs + StringHelper:Unquote (oListItemPairsNode:getText ()) .

            oListItemPairsNode = oListItemPairsNode:nextSibling () .
        END.

        RETURN cListItemPairs .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the LIST-ITEMS Node and returns a CHR(1) delimited list
                 of the LIST-ITEMS
        Notes:
        @param poViewAsNode The JPNode representing the VIEW-AS Option
        @return The CHR(1) delimited list of LIST-ITEMS
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ParseListItems (poViewAsNode AS JPNode):

        DEFINE VARIABLE cListItems     AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE iListItems     AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE oListItemsNode AS JPNode          NO-UNDO .

        ASSIGN cListItems = "":U .

        ASSIGN oListItemsNode = ProparseHelper:FindChildNodeOfNodeType (poViewAsNode, "LISTITEMS":U):firstChild () .

        DO WHILE VALID-OBJECT (oListItemsNode):

            IF NodeTypes:getTypeName (oListItemsNode:getType()) = "COMMA":U THEN
                cListItems = cListItems + CHR (1) .
            ELSE
                cListItems = cListItems + StringHelper:Unquote (oListItemsNode:getText ()) .

            oListItemsNode = oListItemsNode:nextSibling () .
        END.

        RETURN cListItems .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the RADIO-BUTTONS Node and returns a CHR(1) delimited list
                 of the RADIO-BUTTONS
        Notes:
        @param poViewAsNode The JPNode representing the VIEW-AS Option
        @return The CHR(1) delimited list of RADIO-BUTTONS
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ParseRadioButtons (poViewAsNode AS JPNode):

        DEFINE VARIABLE cRadioButtons     AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE iRadioButtons     AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE oRadioButtonsNode AS JPNode          NO-UNDO .

        ASSIGN cRadioButtons = "":U .

        ASSIGN oRadioButtonsNode = ProparseHelper:FindChildNodeOfNodeType (poViewAsNode, "RADIOBUTTONS":U):firstChild () .

        DO WHILE VALID-OBJECT (oRadioButtonsNode):

            IF NodeTypes:getTypeName (oRadioButtonsNode:getType()) = "COMMA":U THEN
                cRadioButtons = cRadioButtons + CHR (1) .
            ELSE
                cRadioButtons = cRadioButtons + StringHelper:Unquote (oRadioButtonsNode:getText ()) .

            oRadioButtonsNode = oRadioButtonsNode:nextSibling () .
        END.

        RETURN cRadioButtons .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Publishes the a status message
        Notes:
        @param pcStatus The status to publish
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PublishStatus (pcStatus AS CHARACTER):

        THIS-OBJECT:OnStatusChanged (NEW StatusChangedEventArgs (pcStatus)) .

    END METHOD .


END CLASS.
