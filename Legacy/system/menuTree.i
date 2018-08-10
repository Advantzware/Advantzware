/* menuTree.i */

DEFINE VARIABLE hFocus AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttMenuTree NO-UNDO
    FIELD treeOrder  AS INTEGER
    FIELD level      AS INTEGER
    FIELD isMenu     AS LOGICAL
    FIELD isOpen     AS LOGICAL
    FIELD isVisible  AS LOGICAL
    FIELD treeParent AS CHARACTER
    FIELD treeChild  AS CHARACTER
    FIELD treeText   AS CHARACTER
    FIELD treeImage  AS CHARACTER
    FIELD hLevel     AS HANDLE
    FIELD hImage     AS HANDLE
    FIELD hEditor    AS HANDLE
        INDEX ttMenuTree IS PRIMARY
            treeOrder
            .
CREATE WIDGET-POOL "MenuTreePool".
SESSION:SET-WAIT-STATE("").

PROCEDURE pClickMenuTree:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE rRowID AS ROWID     NO-UNDO.
    DEFINE VARIABLE cImage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttMenuTree FOR ttMenuTree.
    
    RUN pSetFocus.
    rRowID = TO-ROWID(iphWidget:PRIVATE-DATA).
    FIND FIRST ttMenuTree WHERE ROWID(ttMenuTree) EQ rRowID.
    IF ttMenuTree.isMenu THEN DO:
        FOR EACH bttMenuTree
            WHERE ROWID(bttMenuTree) NE rRowID
              AND bttMenuTree.isMenu EQ YES
              AND bttMenuTree.isOpen EQ YES
              AND bttMenuTree.level  GE ttMenuTree.level
            :
            bttMenuTree.hLevel:LOAD-IMAGE(SEARCH("Graphics\16x16\navigate_right.png")).
            bttMenuTree.isOpen = NO.
        END. /* each bttmenutree */
        ASSIGN
            ttMenuTree.isOpen = NOT ttMenuTree.isOpen
            cImage = "Graphics\16x16\"
                   + IF ttMenuTree.isOpen THEN "navigate_down.png"
                     ELSE "navigate_right.png"
                   .
        ttMenuTree.hLevel:LOAD-IMAGE(cImage).
        RUN pDisplayMenuTree (
            iphWidget:FRAME:HANDLE,
            ttMenuTree.treeChild,
            ttMenuTree.isOpen,
            ttMenuTree.level
            ).
    END. /* if ismenu */
    FOR EACH bttMenuTree
        WHERE bttMenuTree.level GE ttMenuTree.level
        :
        bttMenuTree.hEditor:FONT = ?.
    END. /* each bttMenuTree */
    FOR EACH bttMenuTree
        WHERE bttMenuTree.level  LT ttMenuTree.level
          AND bttMenuTree.isMenu EQ NO
        :
        bttMenuTree.hEditor:FONT = ?.
    END. /* each bttMenuTree */
    ttMenuTree.hEditor:FONT = 6.
    RUN pProcessClick NO-ERROR.

END PROCEDURE.

PROCEDURE pCreatettMenuTree:
    DEFINE INPUT PARAMETER iphFrame  AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrder  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiLevel  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iplMenu   AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcParent AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcChild  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcText   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcImage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.
    
    CREATE ttMenuTree.
    ASSIGN
        ttMenuTree.treeOrder  = ipiOrder
        ttMenuTree.level      = ipiLevel
        ttMenuTree.isMenu     = iplMenu
        ttMenuTree.treeParent = ipcParent
        ttMenuTree.treeChild  = ipcChild
        ttMenuTree.treeText   = ipcText
        ttMenuTree.treeImage  = ipcImage
        .
    
    IF ttMenuTree.isMenu THEN DO:
        CREATE IMAGE hWidget IN WIDGET-POOL "MenuTreePool"
            ASSIGN
                FRAME = iphFrame
                NAME = STRING(ipiOrder)
                COL = 1
                ROW = 1
                SENSITIVE = YES
                WIDTH = 4.8
                HEIGHT = 1.14
                TRANSPARENT = YES
                PRIVATE-DATA = STRING(ROWID(ttMenuTree))                
          TRIGGERS:
            ON MOUSE-SELECT-CLICK
              PERSISTENT RUN pClickMenuTree IN THIS-PROCEDURE (hWidget:HANDLE).
          END TRIGGERS.
        IF VALID-HANDLE(hWidget) THEN DO:
            ttMenuTree.hLevel = hWidget. 
            hWidget:LOAD-IMAGE(SEARCH("Graphics\16x16\navigate_right.png")).
        END.
    END. /* if ismenu */
        
    CREATE IMAGE hWidget IN WIDGET-POOL "MenuTreePool"
        ASSIGN
            FRAME = iphFrame
            NAME = STRING(ipiOrder)
            COL = 1
            ROW = 1
            SENSITIVE = YES
            WIDTH = 4.8
            HEIGHT = 1.14
            TRANSPARENT = YES
            PRIVATE-DATA = STRING(ROWID(ttMenuTree))
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClickMenuTree IN THIS-PROCEDURE (hWidget:HANDLE).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ttMenuTree.hImage = hWidget.            
        hWidget:LOAD-IMAGE(SEARCH(ttMenuTree.treeImage)).
    END.

    dWidth = LENGTH(ttMenuTree.treeText) * 1.6.
    IF dWidth LT 6 THEN
    dWidth = 6.
    
    CREATE EDITOR hWidget IN WIDGET-POOL "MenuTreePool"
        ASSIGN
            FRAME = iphFrame
            NAME = STRING(ipiOrder)
            COL = 1
            ROW = 1
            SENSITIVE = YES
            WIDTH = dWidth
            HEIGHT = 1
            FGCOLOR = ?
            BGCOLOR = ?
            SCROLLBAR-HORIZONTAL = NO
            SCROLLBAR-VERTICAL = NO
            WORD-WRAP = NO
            READ-ONLY = YES
            BOX = NO
            SCREEN-VALUE = ttMenuTree.treeText
            PRIVATE-DATA = STRING(ROWID(ttMenuTree))
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClickMenuTree IN THIS-PROCEDURE (hWidget:HANDLE).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ttMenuTRee.hEditor = hWidget.
        hWidget:LOAD-MOUSE-POINTER("ARROW").
    END.

END PROCEDURE.

PROCEDURE pDisplayMenuTree:
    DEFINE INPUT PARAMETER iphFrame   AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcParent  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplVisible AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiLevel   AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE dCol AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow AS DECIMAL NO-UNDO INITIAL 1.5.
    DEFINE VARIABLE i    AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bttMenuTree FOR ttMenuTree.

    SESSION:SET-WAIT-STATE("General").
    IF INDEX(THIS-PROCEDURE:INTERNAL-ENTRIES,"LockWindowUpdate") NE 0 THEN
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    /* make frame large enough for expanding nodes */
    iphFrame:VIRTUAL-HEIGHT = 320.

    FOR EACH bttMenuTree
        WHERE bttMenuTree.level     GT ipiLevel
          AND bttMenuTree.isVisible EQ YES
        :
        ASSIGN
            bttMenuTree.hImage:HIDDEN  = YES
            bttMenuTree.hEditor:HIDDEN = YES
            bttMenuTree.isVisible      = NO
            .
        IF VALID-HANDLE(bttMenuTree.hLevel) THEN
        bttMenuTree.hLevel:HIDDEN = YES.
    END. /* each bttMenuTree */
    
    FOR EACH bttMenuTree
        WHERE bttMenuTree.treeParent EQ ipcParent
        :
        bttMenuTree.isVisible = iplVisible.
    END. /* each bttMenuTree */

    FOR EACH bttMenuTree
        WHERE bttMenuTree.isVisible EQ YES
        :
        dCol = (bttMenuTree.level - 1) * 6 + 3.
        IF bttMenuTree.isMenu THEN DO:
            ASSIGN
                bttMenuTree.hLevel:COL = dCol
                bttMenuTree.hLevel:ROW = dRow
                bttMenuTree.hLevel:HIDDEN = NO
                dCol = bttMenuTree.hLevel:COL
                     + bttMenuTree.hLevel:WIDTH
                     .
        END. /* if ismenu */
        ASSIGN
            bttMenuTree.hImage:COL = dCol
            bttMenuTree.hImage:ROW = dRow
            bttMenuTree.hImage:HIDDEN = NO
            dCol = bttMenuTree.hImage:COL
                 + bttMenuTree.hImage:WIDTH
                 .
        /* check to be sure editor widget fits in frame */
        IF dCol + bttMenuTree.hEditor:WIDTH GE iphFrame:WIDTH THEN
        bttMenuTree.hEditor:WIDTH = bttMenuTree.hEditor:WIDTH
                                  - (dCol + bttMenuTree.hEditor:WIDTH
                                  -  iphFrame:WIDTH)
                                  .
        ASSIGN
            bttMenuTree.hEditor:COL = dCol
            bttMenuTree.hEditor:ROW = dRow
            bttMenuTree.hEditor:HIDDEN = NO
            dRow = dRow + 1.19
            .
    END. /* each bttMenuTree */
    
    /* adjust frame height for what is visible */
    FIND LAST bttMenuTree
         WHERE bttMenuTree.isVisible EQ YES
         NO-ERROR.
    IF AVAILABLE bttMenuTree THEN DO:
        dRow = bttMenuTree.hEditor:ROW
             + bttMenuTree.hEditor:HEIGHT + 1.    
        IF dRow LT iphFrame:HEIGHT THEN
        dRow = iphFrame:HEIGHT.
        iphFrame:VIRTUAL-HEIGHT = dRow.
    END. /* if avail */

    IF INDEX(THIS-PROCEDURE:INTERNAL-ENTRIES,"LockWindowUpdate") NE 0 THEN
    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

PROCEDURE pInitMenuTree:
    FOR EACH ttMenuTree:
        IF VALID-HANDLE(ttMenuTree.hLevel) THEN
        DELETE OBJECT ttMenuTree.hLevel.
        IF VALID-HANDLE(ttMenuTree.hImage) THEN
        DELETE OBJECT ttMenuTree.hImage.
        IF VALID-HANDLE(ttMenuTree.hEditor) THEN
        DELETE OBJECT ttMenuTree.hEditor.
        DELETE ttMenuTree.
    END. /* each ttsysctrl */

END PROCEDURE.

PROCEDURE pSetFocus:
    IF VALID-HANDLE(hFocus) THEN DO:
        hFocus:SCREEN-VALUE = "".
        APPLY "ENTRY":U TO hFocus.
    END.

END PROCEDURE.
