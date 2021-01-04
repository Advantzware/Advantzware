/* menuTree.i - rstark 8.1.2018 */
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
DEFINE VARIABLE lMenuImage    AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE iFont         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cMenuCreated  AS CHARACTER NO-UNDO.

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
    FIELD hRectangle    AS HANDLE
    FIELD lWidgetExist  AS LOGICAL 
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
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE rRowID AS ROWID     NO-UNDO.
    DEFINE VARIABLE cImage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttMenuTree FOR ttMenuTree.
    
    rRowID = TO-ROWID(ENTRY(2,iphWidget:PRIVATE-DATA)).

    FIND FIRST ttMenuTree
         WHERE ROWID(ttMenuTree) EQ rRowID
         NO-ERROR.
    IF AVAILABLE ttMenuTree AND 
       ttMenuTree.treeChild EQ "Exit" THEN DO:
        APPLY 'WINDOW-CLOSE':U TO hWindow.
        RETURN.
    END. /* if exit */
    IF ttMenuTree.isMenu THEN DO:
        FOR EACH bttMenuTree
            WHERE ROWID(bttMenuTree) NE rRowID
              AND bttMenuTree.isMenu EQ YES
              AND bttMenuTree.isOpen EQ YES
              AND bttMenuTree.level  GE ttMenuTree.level
            :
            IF NOT bttMenuTree.lWidgetExist THEN NEXT.
            bttMenuTree.hLevel:LOAD-IMAGE(SEARCH(cImageFolder + "plus.ico")).
            bttMenuTree.isOpen = NO.
        END. /* each bttmenutree */
        IF ttMenuTree.isOpen THEN
        cMnemonic = SUBSTR(cMnemonic,1,LENGTH(cMnemonic) - 1).
        ASSIGN
            ttMenuTree.isOpen = NOT ttMenuTree.isOpen
            cImage = SEARCH(cImageFolder
                   + IF ttMenuTree.isOpen THEN "minus.ico"
                     ELSE "plus.ico")
                   .
        ttMenuTree.hLevel:LOAD-IMAGE(cImage).
        
        RUN pDisplayMenuTree (
            iphWidget:FRAME:HANDLE,
            ttMenuTree.treeChild,
            ttMenuTree.isOpen,
            ttMenuTree.level
            ).
            
        FOR FIRST ttMenuTree WHERE ROWID(ttMenuTree) EQ rRowID :
            FOR EACH bttMenuTree
                WHERE ROWID(bttMenuTree) NE rRowID
                  AND bttMenuTree.isMenu EQ YES
                  AND bttMenuTree.isOpen EQ YES
                  AND bttMenuTree.level  LT ttMenuTree.level
                :
                IF NOT bttMenuTree.lWidgetExist THEN NEXT.
                bttMenuTree.hLevel:LOAD-IMAGE(SEARCH(cImageFolder + "minus.ico")).
            END. /* each bttmenutree */
            cImage = SEARCH(cImageFolder
                   + IF ttMenuTree.isOpen THEN "minus.ico"
                     ELSE "plus.ico").
            ttMenuTree.hLevel:LOAD-IMAGE(cImage).
        END.
    END. /* if ismenu */
    FIND FIRST ttMenuTree WHERE ROWID(ttMenuTree) EQ rRowID NO-ERROR.

    FOR EACH bttMenuTree
        WHERE bttMenuTree.level        GE ttMenuTree.level
          AND bttMenuTree.lWidgetExist EQ YES
        :
        ASSIGN
            bttMenuTree.hEditor:FONT    = ?
            &IF DEFINED(mainMenuBGColor) NE 0 &THEN
            bttMenuTree.hEditor:FONT    = iFont
            bttMenuTree.hEditor:BGCOLOR = iEditorBGColor
            bttMenuTree.hEditor:FGCOLOR = ?
            &ENDIF
            .
    END. /* each bttMenuTree */

    FOR EACH bttMenuTree
        WHERE bttMenuTree.level        LT ttMenuTree.level
          AND bttMenuTree.isMenu       EQ NO
          AND bttMenuTree.lWidgetExist EQ YES
        :
        ASSIGN
            bttMenuTree.hEditor:FONT    = ?
            &IF DEFINED(mainMenuBGColor) NE 0 &THEN
            bttMenuTree.hEditor:FONT    = iFont
            bttMenuTree.hEditor:BGCOLOR = iEditorBGColor
            bttMenuTree.hEditor:FGCOLOR = ?
            &ENDIF
            .
    END. /* each bttMenuTree */

    FIND FIRST ttMenuTree
         WHERE ROWID(ttMenuTree) EQ rRowID
         NO-ERROR.
    IF AVAILABLE ttMenuTree THEN    
    ASSIGN
        ttMenuTree.hEditor:FONT    = 22
        &IF DEFINED(mainMenuBGColor) NE 0 &THEN
        ttMenuTree.hEditor:FONT    = iFont + 1
        ttMenuTree.hEditor:BGCOLOR = iBGColor[ttMenuTree.level] 
        ttMenuTree.hEditor:FGCOLOR = iFGColor[ttMenuTree.level]
        &ENDIF
        .
    RUN pSetFocus.

    IF INDEX(THIS-PROCEDURE:INTERNAL-ENTRIES,"pProcessClick") NE 0 THEN
    RUN pProcessClick NO-ERROR.

END PROCEDURE.

PROCEDURE pCreatettMenuTree:
    DEFINE INPUT PARAMETER iphFrame                AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrder                AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiLevel                AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iplMenu                 AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcParent               AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcChild                AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcText                 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcImage                AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMnemonic             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShowMnemonic         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPositionMneminic     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplActive               AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcCreateWidgetOfParent AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cTreeText AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMnemonic AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hWidget   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE dWidth    AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE cImage AS CHARACTER NO-UNDO.
    
    IF ipcCreateWidgetOfParent EQ "file" THEN DO:       
        FIND FIRST ttMenuTree
             WHERE ttMenuTree.mnemonic EQ ipcMnemonic
             NO-ERROR.
        IF NOT AVAILABLE ttMenuTree THEN DO:
            CREATE ttMenuTree.
            ASSIGN
                ttMenuTree.treeOrder  = ipiOrder
                ttMenuTree.level      = ipiLevel
                ttMenuTree.isMenu     = iplMenu
                ttMenuTree.treeParent = ipcParent
                ttMenuTree.treeChild  = ipcChild
                ttMenuTree.treeText   = ipcText
                ttMenuTree.baseText   = fTranslate(ENTRY(1,ipcText),NO)
                ttMenuTree.treeImage  = ipcImage
                ttMenuTree.mnemonic   = ipcMnemonic
                ttMenuTree.isActive   = iplActive        
                .    
        END. /* if not avail */
    END. /* if file */
    FOR EACH ttMenuTree
        WHERE ttMenuTree.treeParent   EQ ipcCreateWidgetOfParent 
          AND ttMenuTree.lWidgetExist EQ NO
        :
        ttMenuTree.lWidgetExist = YES.    
        CREATE RECTANGLE hWidget IN WIDGET-POOL cMenuTreePool
            ASSIGN
            FRAME = iphFrame
            NAME = STRING(ipiOrder)
            COL = 1
            ROW = 1
            WIDTH = dObjectWidth
            HEIGHT = dObjectHeight
            HIDDEN = YES
            BGCOLOR = iEditorBGColor
            FGCOLOR = iEditorBGColor
            .
        IF VALID-HANDLE(hWidget) THEN 
            ttMenuTree.hRectangle = hWidget.   
        IF ttMenuTree.isMenu THEN DO:
            CREATE IMAGE hWidget IN WIDGET-POOL cMenuTreePool
                ASSIGN
                    FRAME = iphFrame
                    NAME = STRING(ipiOrder)
                    COL = 1
                    ROW = 1
                    SENSITIVE = YES
                    WIDTH = dObjectWidth
                   HEIGHT = dObjectHeight
                    TRANSPARENT = YES
                    PRIVATE-DATA = "," + STRING(ROWID(ttMenuTree))                
              TRIGGERS:
                ON MOUSE-SELECT-CLICK
                  PERSISTENT RUN pClickMenuTree IN THIS-PROCEDURE (hWidget:HANDLE).
              END TRIGGERS.
            IF VALID-HANDLE(hWidget) THEN DO:
                ASSIGN
                    ttMenuTree.hLevel = hWidget. 
                    cImage = SEARCH(cImageFolder
                        + IF ipcCreateWidgetOfParent EQ "file" THEN "plus.ico"
                          ELSE "minus.ico")
                        .
                hWidget:LOAD-IMAGE(cImage).
            END. /* if valid-handle */
            ASSIGN
                hWidget:HEIGHT-PIXELS = 16
                hWidget:WIDTH-PIXELS  = 16
                .
        END. /* if ismenu */

        IF lMenuImage THEN DO:            
            CREATE IMAGE hWidget IN WIDGET-POOL cMenuTreePool
                ASSIGN
                    FRAME = iphFrame
                    NAME = STRING(ipiOrder)
                    COL = 1
                    ROW = 1
                    SENSITIVE = YES
                    HIDDEN = YES
                  WIDTH = dObjectWidth
                    HEIGHT = dObjectHeight
                    TRANSPARENT = YES
                RETAIN-SHAPE = TRUE
                    PRIVATE-DATA = "," + STRING(ROWID(ttMenuTree))
              TRIGGERS:
                ON MOUSE-SELECT-CLICK
                  PERSISTENT RUN pClickMenuTree IN THIS-PROCEDURE (hWidget:HANDLE).
              END TRIGGERS.
            IF VALID-HANDLE(hWidget) THEN DO:
                ttMenuTree.hImage = hWidget.
        
                
                hWidget:LOAD-IMAGE(SEARCH(cImageFolder + ttMenuTree.treeImage)).
            END.
        END. /* if lmenuimage */
    
        ASSIGN
            dWidth    = LENGTH(fMnemonic(ttMenuTree.mnemonic) + ttMenuTree.treeText) * 1.6
            cTreeText = fTreeText(
                ttMenuTree.isMenu,
                fTranslate(ttMenuTree.treeText,NO),
                ttMenuTree.mnemonic,
                ipcShowMnemonic,
                ipcPositionMneminic
                ) 
                .
               
        IF dWidth LT 6 THEN
        dWidth = 6.
        
        CREATE EDITOR hWidget IN WIDGET-POOL cMenuTreePool
            ASSIGN
                FRAME = iphFrame
                NAME = STRING(ipiOrder)
                COL = 1
                ROW = 1
                SENSITIVE = YES
                HIDDEN = YES
                WIDTH = dWidth
                HEIGHT = dObjectHeight
                &IF DEFINED(mainMenuBGColor) NE 0 &THEN
                FONT = iFont
                &ENDIF
                FGCOLOR = 0
                BGCOLOR = iEditorBGColor
                SCROLLBAR-HORIZONTAL = NO
                SCROLLBAR-VERTICAL = NO
                WORD-WRAP = NO
                READ-ONLY = YES
                BOX = NO
                SCREEN-VALUE = cTreeText
                PRIVATE-DATA = ttMenuTree.treeText + "," + STRING(ROWID(ttMenuTree))
          TRIGGERS:
            ON MOUSE-SELECT-CLICK
              PERSISTENT RUN pClickMenuTree IN THIS-PROCEDURE (hWidget:HANDLE).
          END TRIGGERS.
        IF VALID-HANDLE(hWidget) THEN DO:
            ASSIGN
                ttMenuTRee.hEditor = hWidget
                hWidget:TOOLTIP    = IF ipcMnemonic NE "" THEN "HotKey: " + ipcMnemonic
                                     ELSE hWidget:SCREEN-VALUE
                .
            hWidget:LOAD-MOUSE-POINTER("GLOVE").
        END.
        
        &IF "{&isActive}" EQ "YES" &THEN
        CREATE TOGGLE-BOX hWidget IN WIDGET-POOL cMenuTreePool
            ASSIGN
                FRAME = iphFrame
                NAME = STRING(ipiOrder)
                COL = 1
                ROW = 1
                SENSITIVE = YES
                HIDDEN = YES
                WIDTH = 3
                HEIGHT = dObjectHeight
                FGCOLOR = 1
                BGCOLOR = ?
                SCREEN-VALUE = STRING(ttMenuTree.isActive)
                PRIVATE-DATA = "," + STRING(ROWID(ttMenuTree))
          TRIGGERS:
            ON VALUE-CHANGED
              PERSISTENT RUN pToggle IN THIS-PROCEDURE (hWidget:HANDLE).
          END TRIGGERS.
        IF VALID-HANDLE(hWidget) THEN
        ttMenuTRee.hToggle = hWidget.
        &ENDIF
    
    END.
    
    IF NOT VALID-HANDLE(hWindow) THEN 
    hWindow = CURRENT-WINDOW.

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

    IF ipcParent NE "file" AND
       LOOKUP(ipcParent, cMenuCreated,"," ) LE 0 THEN DO:       
        RUN pBuildttMenuTree(ipcParent).
        RUN pMenuSize IN THIS-PROCEDURE NO-ERROR.
    END. /* if ne file */
        
    ASSIGN
        cMenuCreated = cMenuCreated + "," + ipcParent        
        /* make frame large enough for expanding nodes */
        iphFrame:VIRTUAL-HEIGHT = 320
        .
    FOR EACH bttMenuTree
        WHERE bttMenuTree.level        GT ipiLevel
          AND bttMenuTree.isVisible    EQ YES
          AND bttMenuTree.lWidgetExist EQ YES
        :
        IF VALID-HANDLE(bttMenuTree.hImage) THEN
        bttMenuTree.hImage:HIDDEN = YES.
        ASSIGN       
            bttMenuTree.hEditor:HIDDEN    = YES
            bttMenuTree.hRectangle:HIDDEN = YES
            bttMenuTree.isVisible         = NO
            .
        IF VALID-HANDLE(bttMenuTree.hLevel) THEN
        bttMenuTree.hLevel:HIDDEN = YES.
        IF VALID-HANDLE(bttMenuTree.hToggle) THEN
        bttMenuTree.hToggle:HIDDEN = YES.
    END. /* each bttMenuTree */
    
    FOR EACH bttMenuTree
        WHERE bttMenuTree.treeParent EQ ipcParent AND bttMenuTree.lWidgetExist
        :
        bttMenuTree.isVisible = iplVisible.
    END. /* each bttMenuTree */

    FOR EACH bttMenuTree
        WHERE  bttMenuTree.isVisible   EQ YES
          AND (bttMenuTree.isActive    EQ YES
           OR  lToggle                 EQ YES)
          AND bttMenuTree.lWidgetExist EQ YES
        :     
        ASSIGN
            dCol                          = (bttMenuTree.level - 1) * 6 + 3  
            bttMenuTree.hRectangle:WIDTH  = iphFrame:WIDTH - dCol - 5
            bttMenuTree.hRectangle:COL    = dcol
            bttMenuTree.hRectangle:ROW    = dRow
            bttMenuTree.hRectangle:HIDDEN = NO
            .
        IF bttMenuTree.isMenu THEN
        ASSIGN
            bttMenuTree.hLevel:COL    = dCol
            bttMenuTree.hLevel:ROW    = dRow
            bttMenuTree.hLevel:HIDDEN = NO
            dCol = bttMenuTree.hLevel:COL
                 + bttMenuTree.hLevel:WIDTH + 1.5
                 .
        &IF "{&isActive}" EQ "YES" &THEN
        IF lToggle THEN 
        ASSIGN
            bttMenuTree.hToggle:COL    = dCol
            bttMenuTree.hToggle:ROW    = dRow
            bttMenuTree.hToggle:HIDDEN = NO
            dCol = bttMenuTree.hToggle:COL
                 + bttMenuTree.hToggle:WIDTH
                 .
        ELSE
        &ENDIF
        IF lMenuImage AND VALID-HANDLE(bttMenuTree.hImage)THEN 
        ASSIGN
            bttMenuTree.hImage:COL    = dCol
            bttMenuTree.hImage:ROW    = dRow
            bttMenuTree.hImage:HIDDEN = NO
            dCol = bttMenuTree.hImage:COL
                 + bttMenuTree.hImage:WIDTH + 1.5
                 .            
        ASSIGN
            bttMenuTree.hEditor:WIDTH  = iphFrame:WIDTH - dCol - 5
            bttMenuTree.hEditor:COL    = dCol
            bttMenuTree.hEditor:ROW    = dRow
            bttMenuTree.hEditor:HIDDEN = NO
            dRow = dRow + bttMenuTree.hEditor:HEIGHT + 0.15
            .
    END. /* each bttMenuTree */

    /* adjust frame height for what is visible */
    FIND LAST bttMenuTree
         WHERE  bttMenuTree.isVisible EQ YES
           AND (bttMenuTree.isActive  EQ YES
           OR   lToggle               EQ YES)
         NO-ERROR.
    IF AVAILABLE bttMenuTree THEN DO:
        dRow = bttMenuTree.hEditor:ROW
             + bttMenuTree.hEditor:HEIGHT + 1
             .    
        IF dRow LT iphFrame:HEIGHT THEN
        dRow = iphFrame:HEIGHT.
        iphFrame:VIRTUAL-HEIGHT = dRow.
    END. /* if avail */

    IF INDEX(THIS-PROCEDURE:INTERNAL-ENTRIES,"LockWindowUpdate") NE 0 THEN
    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

PROCEDURE pInitMenuTree:
    cMenuCreated = "".
    FOR EACH ttMenuTree:
        IF VALID-HANDLE(ttMenuTree.hLevel) THEN
        DELETE OBJECT ttMenuTree.hLevel.
        IF VALID-HANDLE(ttMenuTree.hRectangle) THEN
        DELETE OBJECT ttMenuTree.hRectangle.
        IF VALID-HANDLE(ttMenuTree.hImage) THEN
        DELETE OBJECT ttMenuTree.hImage.
        IF VALID-HANDLE(ttMenuTree.hEditor) THEN
        DELETE OBJECT ttMenuTree.hEditor.
        IF VALID-HANDLE(ttMenuTree.hToggle) THEN
        DELETE OBJECT ttMenuTree.hToggle.
        DELETE ttMenuTree.
    END. /* each ttsysctrl */

END PROCEDURE.

PROCEDURE pKeyPress:
&Scoped-define validKeys ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()

    DEFINE INPUT PARAMETER iphFrame   AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER ipiLastKey AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cSave AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKey  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx   AS INTEGER   NO-UNDO.

    IF ipiLastKey EQ 32 THEN DO:
        cMnemonic = SUBSTR(cMnemonic,1,1).
        RELEASE ttMenuTree.
    END. /* if 32 */
    ELSE IF LENGTH(cMnemonic) GE 2 AND ipiLastKey EQ 48 THEN DO:
        RUN spSetTaskFilter (cMnemonic, "", "").
        RUN Get_Procedure IN Persistent-Handle("dynTasks.", OUTPUT run-proc, YES).
        RETURN.
    END. /* zero */
    ELSE
    ASSIGN
        cSave     = TRIM(SUBSTR(cMnemonic,1,2))
        cKey      = CAPS(KEYLABEL(ipiLastKey)) WHEN INDEX("{&validKeys}",KEYLABEL(ipiLastKey)) NE 0
        cMnemonic = IF cKey NE "" THEN cSave + cKey ELSE ""
        .
    DO WHILE cMnemonic NE "":
        idx = idx + 1.
        /* prevents endless loop, resets menu */
        IF idx GE 4 THEN DO:
            FIND FIRST ttMenuTree
                 WHERE ttMenuTree.isActive EQ YES
                   AND ttMenuTree.isOpen   EQ YES
                 NO-ERROR.
            IF AVAILABLE ttMenuTree THEN
            RUN pClickMenuTree (ttMenuTree.hEditor).
            LEAVE.
        END. /* idx ge 4 */
        FIND FIRST ttMenuTree
             WHERE  ttMenuTree.mnemonic EQ cMnemonic
               AND (ttMenuTree.isActive EQ YES
                OR  lToggle             EQ YES)
             NO-ERROR.
        IF AVAILABLE ttMenuTree THEN DO:
            RUN pClickMenuTree (ttMenuTree.hEditor).
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

PROCEDURE pSetFocus:
    DEFINE VARIABLE dRow AS DECIMAL NO-UNDO.
    DEFINE VARIABLE i    AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bttMenuTree FOR ttMenuTree.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    /* this forces frame to auto adjust to selection */
    IF VALID-HANDLE(hFocus) THEN DO:
        IF AVAILABLE ttMenuTree THEN DO:
            FIND LAST bttMenuTree
                 WHERE bttMenuTree.treeParent EQ ttMenuTree.treeChild
                 NO-ERROR.
            /* position to last item in menu so user can see them */
            dRow = IF AVAILABLE bttMenuTree THEN bttMenuTree.hEditor:ROW
                   ELSE ttMenuTree.hEditor:ROW.
            /* prevent position outside of frame */
            IF dRow GT hFocus:FRAME:VIRTUAL-HEIGHT - 1 THEN
            dRow = hFocus:FRAME:VIRTUAL-HEIGHT - 1.
            hFocus:ROW = dRow.
            APPLY "ENTRY":U TO ttMenuTree.hEditor. 
        END.
        /* prevents cursor from blinking in editor object */
        hFocus:SCREEN-VALUE = "".
        APPLY "ENTRY":U TO hFocus.
    END.

    RUN LockWindowUpdate (0,OUTPUT i).
    
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
            bttMenuTree.hToggle:SCREEN-VALUE = STRING(iplActive)
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
            bttMenuTree.hToggle:SCREEN-VALUE = "YES"
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
    FIND FIRST ttMenuTree
         WHERE ROWID(ttMenuTree) EQ rRowID.
    ttMenuTree.isActive = lActive.
    IF ttMenuTree.isMenu THEN
    RUN pSetisActive (ttMenuTree.treeChild, lActive).
    IF lActive THEN
    RUN pSetParentToggle (ttMenuTree.treeParent).

END PROCEDURE.
