/* menuTree_FWD.i - rstark 8.1.2018 */
/*
Usage:
    1. place this include {system/menuTree.i} in definitions section
    2. create PROCEDURE pBuildttMenuTree to populate TEMP-TABLE ttMenuTree
        a. populate ttMenuTree using the following
           RUN pCreatettMenuTree (
               FRAME {&FRAME-NAME}:HANDLE,
               iOrder,
               iLevel,
               lMenu,
               cParent,
               cChild,
               cText,
               cImage,
               cMnemonic,
               cShowMnemonic,
               cPositionMnemonic,
               lActive
               ).
    3. create PROCEDURE pProcessClick for processing after a menu item click
    4. to prevent cursor from residing inside the editor object, create a
       dummy object fill-in in AppBuilder, such as svFocus
        DEFINE VARIABLE svFocus AS CHARACTER FORMAT "X(256)":U
             VIEW-AS FILL-IN
             SIZE .2 BY 1
             BGCOLOR 8  NO-UNDO.
        a. in the Main Block section add
           hFocus = svFocus:HANDLE
*/
DEFINE VARIABLE hFocus        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hWindow       AS HANDLE    NO-UNDO.
DEFINE VARIABLE lToggle       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMnemonic     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMenuTreePool AS CHARACTER NO-UNDO INITIAL "MenuTreePool".
DEFINE VARIABLE cImageFolder  AS CHARACTER NO-UNDO INITIAL "Graphics/16X16/".
DEFINE VARIABLE dObjectHeight AS DECIMAL   NO-UNDO INITIAL 1.14.
DEFINE VARIABLE dObjectWidth  AS DECIMAL   NO-UNDO INITIAL 4.8.
DEFINE VARIABLE iMenuSize     AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE iLanguage     AS INTEGER   NO-UNDO INITIAL 1.

DEFINE VAR hTree AS HANDLE.

DEFINE TEMP-TABLE ttMenuTree NO-UNDO
    FIELD treeOrder     AS INTEGER
    FIELD level         AS INTEGER
    FIELD isMenu        AS LOGICAL
    FIELD isOpen        AS LOGICAL
    FIELD isVisible     AS LOGICAL
    FIELD treeParent    AS CHARACTER
    FIELD treeChild     AS CHARACTER
    FIELD treeText      AS CHARACTER
    FIELD baseText      AS CHARACTER
    FIELD treeImage     AS CHARACTER
    FIELD mnemonic      AS CHARACTER
    FIELD isActive      AS LOGICAL
    FIELD favorite      AS LOGICAL
    FIELD favoriteOrder AS DECIMAL
    FIELD hLevel        AS HANDLE
    FIELD hImage        AS HANDLE
    FIELD hEditor       AS HANDLE
    FIELD hToggle       AS HANDLE
        INDEX ttMenuTree IS PRIMARY
            treeOrder
        INDEX ttMnemonic
            mnemonic
        INDEX ttActive
            isActive
            .
/* ensure widget pool is unique for each instance of a menu tree */
cMenuTreePool = cMenuTreePool + STRING(TIME,"99999").
CREATE WIDGET-POOL cMenuTreePool.
SESSION:SET-WAIT-STATE("").

{system/translations.i}

/* ************************  Function Implementations ***************** */

FUNCTION fMnemonic RETURNS CHARACTER
    (ipcMnemonic AS CHARACTER):
    RETURN IF ipcMnemonic EQ "" THEN "" ELSE  "[" + ipcMnemonic + "]".

END FUNCTION.

FUNCTION fTreeText RETURNS CHARACTER
	(iplIsMenu AS LOGICAL,
	 ipcText AS CHARACTER,
	 ipcMnemonic AS CHARACTER,
	 ipcShowMnemonic AS CHARACTER,
	 ipcPositionMnemonic AS CHARACTER
	):

    DEFINE VARIABLE cTreeText AS CHARACTER NO-UNDO.

    CASE ipcShowMnemonic:
        WHEN "None" OR WHEN "" THEN
        cTreeText = ipcText.
        WHEN "All" THEN
            CASE ipcPositionMnemonic:
                WHEN "Begin" THEN
                cTreeText = fMnemonic(ipcMnemonic) + " " + ipcText.
                WHEN "End" THEN
                cTreeText = ipcText + " " + fMnemonic(ipcMnemonic).
            END CASE.
        WHEN "Program" THEN
            IF iplIsMenu THEN cTreeText = ipcText.
            ELSE
            CASE ipcPositionMnemonic:
                WHEN "Begin" THEN
                cTreeText = fMnemonic(ipcMnemonic) + " " + ipcText.
                WHEN "End" THEN
                cTreeText = ipcText + " " + fMnemonic(ipcMnemonic).
            END CASE.
    END CASE.

	RETURN cTreeText.

END FUNCTION.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pClickMenuTree:
    DEFINE VARIABLE hNode  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE rRowID AS ROWID     NO-UNDO.
    DEFINE VARIABLE cImage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bttMenuTree FOR ttMenuTree.

    hNode = hTree:SELECTED-NODE.
    if NOT VALID-HANDLE(hNode) THEN RETURN.

    rRowID = TO-ROWID(ENTRY(2,hNode:TREE-NODE-VALUE)).
    FIND FIRST ttMenuTree WHERE ROWID(ttMenuTree) EQ rRowID.
    IF ttMenuTree.treeChild EQ "Exit" THEN DO:
        APPLY 'WINDOW-CLOSE':U TO hWindow.
        RETURN.
    END. /* if exit */
    /*
    IF ttMenuTree.isMenu THEN DO:
        IF hNode:NODE-EXPANDED THEN
        cMnemonic = SUBSTR(cMnemonic,1,LENGTH(cMnemonic) - 1).
                   .
    END. /* if ismenu */
    */
    IF INDEX(THIS-PROCEDURE:INTERNAL-ENTRIES,"pProcessClick") NE 0 THEN
    RUN pProcessClick(hNode) NO-ERROR.

END PROCEDURE.

PROCEDURE pCreatettMenuTree:
    DEFINE INPUT PARAMETER iphFrame            AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrder            AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiLevel            AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iplMenu             AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcParent           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcChild            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcText             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcImage            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMnemonic         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShowMnemonic     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPositionMneminic AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplActive           AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cTreeText AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMnemonic AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hWidget   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE dWidth    AS DECIMAL   NO-UNDO.

    ASSIGN
        cTreeText = fTreeText(
            iplMenu,
            fTranslate(ipcText,NO),
            ipcMnemonic,
            ipcShowMnemonic,
            ipcPositionMneminic
            )
            .

    CREATE ttMenuTree.
    ASSIGN
        ttMenuTree.treeOrder  = ipiOrder
        ttMenuTree.level      = ipiLevel
        ttMenuTree.isMenu     = iplMenu
        ttMenuTree.treeParent = ipcParent
        ttMenuTree.treeChild  = ipcChild
        ttMenuTree.treeText   = cTreeText
        ttMenuTree.baseText   = fTranslate(ENTRY(1,ipcText),NO)
        ttMenuTree.treeImage  = ipcImage
        ttMenuTree.mnemonic   = ipcMnemonic
        ttMenuTree.isActive   = iplActive
        .

    IF NOT VALID-HANDLE(hWindow) THEN
    hWindow = CURRENT-WINDOW.

END PROCEDURE.

PROCEDURE pNodeExpanding:
    DEFINE VARIABLE hNode AS HANDLE NO-UNDO.
    DEFINE VARIABLE rRowID  AS ROWID   NO-UNDO.

    hNode = hTree:trigger-node.
    hTree:collapse-all-except(hNode).
    run pDisplayMenuTree(?, hNode:node-key, true, 0).
end procedure.

PROCEDURE pDisplayMenuTree:
    DEFINE INPUT PARAMETER iphFrame   AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcParent  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplVisible AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiLevel   AS INTEGER   NO-UNDO.

    DEFINE BUFFER bttMenuTree FOR ttMenuTree.
    DEFINE VARIABLE hNode AS HANDLE NO-UNDO.

    if not VALID-HANDLE(hTree) then do:
       create treeview hTree assign
            frame = iphFrame
            col = 1
            row = 1
            width = iphFrame:width
            height = iphFrame:height
            EXPAND-ON-SINGLE-CLICK = true
            visible = true
       TRIGGERS:
          ON DEFAULT-ACTION
             PERSISTENT RUN pClickMenuTree IN THIS-PROCEDURE.
          ON "NODE-EXPANDING"
             PERSISTENT RUN pNodeExpanding IN THIS-PROCEDURE.
       END TRIGGERS.

       RUN pLayoutTree (iphFrame).
    end.

    hTree:building-tree = true.

    FOR EACH bttMenuTree
        WHERE bttMenuTree.treeParent EQ ipcParent
        BY bttMenuTree.treeOrder
        :
        hNode = hTree:find-node(bttMenuTree.treeChild).
        if not valid-handle(hNode) then do:
            def var nodeId as integer.
            hNode = hTree:find-node(ipcParent).
            if (valid-handle(hNode)) then
                nodeId = hNode:node-id.
            else
                nodeId = 0.

            hTree:CREATE-SUB-NODE(nodeId, bttMenuTree.treeText, bttMenuTree.treeChild, bttMenuTree.isMenu, ?, ?, nodeId).
            hNode = hTree:find-node(nodeId).
        end.
        hNode:NODE-TEXT = bttMenuTree.treeText.
        hNode:TREE-NODE-VALUE = bttMenuTree.treeText + "," + STRING(ROWID(bttMenuTree)).
        def var imgId as int.
        imgId = hTree:CREATE-IMAGE(SEARCH(cImageFolder + bttMenuTree.treeImage)).
        hNode:NODE-ICON = imgId.
    END. /* each bttMenuTree */

    hTree:building-tree = false.

END PROCEDURE.

PROCEDURE pInitMenuTree:
    IF VALID-HANDLE(hTree) THEN DO:
        hTree:CLEAR-ALL().
    END.
END PROCEDURE.

PROCEDURE pKeyPress:
&Scoped-define validKeys ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()

    DEFINE INPUT PARAMETER iphFrame   AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER ipiLastKey AS INTEGER NO-UNDO.

    DEFINE VARIABLE cSave AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKey  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx   AS INTEGER   NO-UNDO.

    DEFINE VARIABLE hNode AS HANDLE    NO-UNDO.

    IF ipiLastKey EQ 32 THEN DO:
        cMnemonic = SUBSTR(cMnemonic,1,1).
        RELEASE ttMenuTree.
    END. /* if 32 */
    ELSE
    ASSIGN
        cSave     = SUBSTR(cMnemonic,1,2)
        cKey      = CAPS(KEYLABEL(ipiLastKey)) WHEN INDEX("{&validKeys}",KEYLABEL(ipiLastKey)) NE 0
        cMnemonic = IF cKey NE "" THEN cSave + cKey ELSE ""
        .
    DO WHILE cMnemonic NE "":
        idx = idx + 1.
        /* prevents endless loop */
        IF idx GT 5 THEN LEAVE.
        FIND FIRST ttMenuTree
             WHERE  ttMenuTree.mnemonic EQ cMnemonic
               AND (ttMenuTree.isActive EQ YES
                OR  lToggle             EQ YES)
             NO-ERROR.
        IF AVAILABLE ttMenuTree THEN DO:
            hNode = hTree:find-node(ttMenuTree.treeChild).
            IF VALID-HANDLE(hNode) THEN DO:
                hTree:collapse-all-except(hNode).
                hNode:NODE-EXPANDED = TRUE.
                hTree:SCROLL-NODE-TO-TOP(hNode:node-key).
            END.
            hTree:SELECTED-NODE = hNode.
            /* process events to repaint the selected node before the menu program is executed */
            PROCESS EVENTS.
            run pDisplayMenuTree(?, hNode:node-key, true, 0).
            RUN pClickMenuTree.
            cMnemonic = SUBSTR(cMnemonic,1,2).
            LEAVE.
        END.
        ELSE
        ASSIGN
            cSave     = SUBSTR(cSave,1,LENGTH(cSave) - 1)
            cMnemonic = cSave + cKey
            .
    END. /* while true */

END PROCEDURE.

PROCEDURE pSetisActive:
    DEFINE INPUT PARAMETER ipcTreeChild AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplActive    AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bttMenuTree FOR ttMenuTree.

    FOR EACH bttMenuTree
        WHERE bttMenuTree.treeParent EQ ipcTreeChild
        :
        ASSIGN
            bttMenuTree.isActive = iplActive
            bttMenuTree.hToggle:SCREEN-VALUE = STRING(iplActive) WHEN VALID-HANDLE(bttMenuTree.hToggle)
            .
        IF bttMenuTree.isMenu THEN
        RUN pSetisActive (bttMenuTree.treeChild, iplActive).
    END. /* each bttmenutree */

END PROCEDURE.

PROCEDURE pSetParentToggle:
    DEFINE INPUT PARAMETER ipcParent AS CHARACTER NO-UNDO.

    DEFINE BUFFER bttMenuTree FOR ttMenuTree.

    FIND FIRST bttMenuTree
         WHERE bttMenuTree.treeChild EQ ipcParent
         NO-ERROR.
    IF AVAILABLE bttMenuTree THEN DO:
        ASSIGN
            bttMenuTree.isActive = YES
            bttMenuTree.hToggle:SCREEN-VALUE = "YES" WHEN VALID-HANDLE(bttMenuTree.hToggle)
            .
        RUN pSetParentToggle (bttMenuTree.treeParent).
    END. /* avail ttmenutree */

END PROCEDURE.

PROCEDURE pToggle:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.

    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    DEFINE VARIABLE rRowID  AS ROWID   NO-UNDO.

    ASSIGN
        lActive = iphWidget:SCREEN-VALUE EQ "yes"
        rRowID = TO-ROWID(ENTRY(2,iphWidget:PRIVATE-DATA))
        .
    FIND FIRST ttMenuTree WHERE ROWID(ttMenuTree) EQ rRowID.
    ttMenuTree.isActive = lActive.
    IF ttMenuTree.isMenu THEN
    RUN pSetisActive (ttMenuTree.treeChild, lActive).

    IF lActive THEN
    RUN pSetParentToggle (ttMenuTree.treeParent).

END PROCEDURE.

PROCEDURE pLayoutTree:
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.

    IF VALID-HANDLE(hTree) THEN DO:
        hTree:WIDTH-PIXELS = iphFrame:WIDTH-PIXELS - iphFrame:BORDER-LEFT-PIXELS - iphFrame:BORDER-RIGHT-PIXELS.
        hTree:HEIGHT-PIXELS = iphFrame:HEIGHT-PIXELS - iphFrame:BORDER-TOP-PIXELS - iphFrame:BORDER-BOTTOM-PIXELS.
    END.
END PROCEDURE.
