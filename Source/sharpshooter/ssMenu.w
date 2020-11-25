&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ssMenu.w

  Description: Sharp Shooter Menu

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 10.29.2020

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&Scoped-define parentMenu ssMenu

ON F1 HELP.
ON CTRL-F HELP.
ON CTRL-P HELP.

ON 'CTRL-ALT-D':U ANYWHERE
DO:
    RUN aoa/aoaLauncher.w PERSISTENT ("Dashboard").
    RETURN.
END.

ON 'CTRL-ALT-R':U ANYWHERE
DO:
    RUN aoa/aoaLauncher.w PERSISTENT ("Report").
    RETURN.
END.

ON 'CTRL-ALT-P':U ANYWHERE
DO: 
    RUN util/wPgmrToolbox.w.
END.    

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cLevel            AS CHARACTER NO-UNDO EXTENT 3.
DEFINE VARIABLE cParent           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPositionMnemonic AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgTitle         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShowMnemonic     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLevel            AS INTEGER   NO-UNDO.

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

/* System Constant Values */
{system/sysconst.i}

{system/menuTree.i}
ASSIGN
    cImageFolder      = "Graphics/48X48/"
    dObjectHeight     = 2.62
    dObjectWidth      = 11
    iMenuSize         = 3
    iLanguage         = 1
    lMenuImage        = TRUE
    iFont             = 24
/*    cPositionMnemonic = "Begin"*/
/*    cShowMnemonic     = "All"  */
    .
{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnExit btnBack 
&Scoped-Define DISPLAYED-OBJECTS menuTitle 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBack 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Back" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Exit" 
     SIZE 8 BY 1.91.

DEFINE VARIABLE menuTitle AS CHARACTER FORMAT "X(256)":U INITIAL "Menu Loading..." 
      VIEW-AS TEXT 
     SIZE 52 BY 1.43
     FGCOLOR 15 FONT 24 NO-UNDO.

DEFINE VARIABLE svFocus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE .2 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnExit AT ROW 1 COL 64 WIDGET-ID 4
     btnBack AT ROW 1 COL 2 WIDGET-ID 2
     menuTitle AT ROW 1.24 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71 BY 31.19
         BGCOLOR 21  WIDGET-ID 100.

DEFINE FRAME menuTreeFrame
     svFocus AT ROW 1 COL 1 NO-LABEL WIDGET-ID 82
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.91
         SIZE 71 BY 29.29
         BGCOLOR 15  WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Sharp Shooter Menu"
         COLUMN             = 3
         ROW                = 1.48
         HEIGHT             = 31.19
         WIDTH              = 71
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME menuTreeFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN menuTitle IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME menuTreeFrame
                                                                        */
/* SETTINGS FOR FILL-IN svFocus IN FRAME menuTreeFrame
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME menuTreeFrame
/* Query rebuild information for FRAME menuTreeFrame
     _Query            is NOT OPENED
*/  /* FRAME menuTreeFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sharp Shooter Menu */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sharp Shooter Menu */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBack C-Win
ON CHOOSE OF btnBack IN FRAME DEFAULT-FRAME /* Back */
DO:
    IF cParent NE "" THEN DO:
        RUN pDisplayMenuTree-1 (
            FRAME menuTreeFrame:HANDLE,
            cParent
            ).
        RUN pSetMenuTitle (cParent).
    END.
    ELSE
    APPLY "CHOOSE":U TO btnExit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit C-Win
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME /* Exit */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

ON 'BACKSPACE':U OF FRAME {&FRAME-NAME} ANYWHERE
DO:
    APPLY "CHOOSE":U TO btnBack IN FRAME {&FRAME-NAME}.
END.

ON ANY-PRINTABLE OF FRAME {&FRAME-NAME} ANYWHERE
DO:
    cLevel[iLevel] = KEYLABEL(LASTKEY).
    FIND FIRST ttMenuTree
         WHERE ttMenuTree.mnemonic EQ cLevel[1] + cLevel[2] + cLevel[3]
           AND VALID-HANDLE(ttMenuTree.hImage) EQ YES
         NO-ERROR.
    IF AVAILABLE ttMenuTree THEN
    RUN pClickMenuTree-1 (ttMenuTree.hImage:HANDLE).
    RETURN NO-APPLY.
END.

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND FIRST users NO-LOCK
       WHERE users.user_id EQ USERID("ASI")
       NO-ERROR.
  IF AVAILABLE users THEN
  ASSIGN
      cShowMnemonic     = users.showMnemonic
      cPositionMnemonic = users.positionMnemonic
      .
  RUN enable_UI.
  RUN pBuildttMenuTree.
  RUN pDisplayMenuTree-1 (FRAME menuTreeFrame:HANDLE, "{&parentMenu}").
  RUN pSetMenuTitle ("{&parentMenu}").
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY menuTitle 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnExit btnBack 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY svFocus 
      WITH FRAME menuTreeFrame IN WINDOW C-Win.
  ENABLE svFocus 
      WITH FRAME menuTreeFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-menuTreeFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildttMenuTree C-Win 
PROCEDURE pBuildttMenuTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMnemonic AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lActive   AS LOGICAL NO-UNDO.

    DEFINE BUFFER bPrgrms FOR prgrms.

    EMPTY TEMP-TABLE ttMenuTree.
    
    FIND FIRST bPrgrms NO-LOCK
         WHERE bPrgrms.prgmName EQ "{&parentMenu}"
         NO-ERROR.
    IF NOT AVAILABLE bPrgrms THEN RETURN.
    FOR EACH prgrms NO-LOCK
        WHERE prgrms.menu_item EQ YES
          AND prgrms.menuOrder GE bPrgrms.menuOrder
          AND prgrms.menuLevel GT 0
          AND prgrms.mnemonic  NE ""
           BY prgrms.menuOrder
        :
        lActive = NOT CAN-FIND(FIRST xUserMenu
                               WHERE xUserMenu.user_id  EQ USERID("ASI")
                                 AND xUserMenu.prgmname EQ prgrms.prgmname).
        RUN pCreatettMenuTree-1 (
            FRAME menuTreeFrame:HANDLE,
            prgrms.menuOrder,
            prgrms.menuLevel,
            INDEX(prgrms.prgmname,".") EQ 0,
            prgrms.itemParent,
            prgrms.prgmname,
            (IF prgrms.customMenuTitle NE "" THEN prgrms.customMenuTitle ELSE prgrms.prgTitle),
            prgrms.menuImage[1],
            prgrms.mnemonic,
            cShowMnemonic,
            cPositionMnemonic,
            lActive
            ).
    END. /* each prgrms */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClickMenuTree-1 C-Win 
PROCEDURE pClickMenuTree-1 :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.  

    DEFINE VARIABLE lAccess AS LOGICAL NO-UNDO.
    DEFINE VARIABLE rRowID  AS ROWID   NO-UNDO.

    DEFINE BUFFER bttMenuTree FOR ttMenuTree.
    
    rRowID = TO-ROWID(ENTRY(2,iphWidget:PRIVATE-DATA)).
    FIND FIRST ttMenuTree WHERE ROWID(ttMenuTree) EQ rRowID.
    IF ttMenuTree.isMenu THEN DO:
        RUN pDisplayMenuTree-1 (
            iphWidget:FRAME:HANDLE,
            ttMenuTree.treeChild
            ).
        RUN pSetMenuTitle (ttMenuTree.treeChild).
    END. /* if ismenu */
    ELSE DO:
        /* check module license first before run */
        RUN util/CheckModule.p ("ASI", ttMenuTree.treeChild, YES, OUTPUT lAccess) NO-ERROR.
        IF lAccess THEN 
        RUN Get_Procedure IN Persistent-Handle(ttMenuTree.treeChild,OUTPUT run-proc,YES).
    END. /* if program */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreatettMenuTree-1 C-Win 
PROCEDURE pCreatettMenuTree-1 :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
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
            PRIVATE-DATA = "," + STRING(ROWID(ttMenuTree))
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClickMenuTree-1 IN THIS-PROCEDURE (hWidget:HANDLE).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ttMenuTree.hImage = hWidget.
        IF lMenuImage THEN
        hWidget:LOAD-IMAGE(SEARCH(cImageFolder + ttMenuTree.treeImage)).
    END.

    ASSIGN
        dWidth = LENGTH(fMnemonic(ttMenuTree.mnemonic) + ttMenuTree.treeText) * 1.6
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
            HEIGHT = 1
            FONT = iFont
            FGCOLOR = 1
            BGCOLOR = 26
            SCROLLBAR-HORIZONTAL = NO
            SCROLLBAR-VERTICAL = NO
            WORD-WRAP = NO
            READ-ONLY = YES
            BOX = NO
            SCREEN-VALUE = cTreeText
            PRIVATE-DATA = ttMenuTree.treeText + "," + STRING(ROWID(ttMenuTree))
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClickMenuTree-1 IN THIS-PROCEDURE (hWidget:HANDLE).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ASSIGN
            ttMenuTree.hEditor = hWidget
            hWidget:TOOLTIP    = IF ipcMnemonic NE "" THEN "HotKey: " + ipcMnemonic
                                 ELSE hWidget:SCREEN-VALUE
            .
        hWidget:LOAD-MOUSE-POINTER("GLOVE").
    END.
    
    CREATE RECTANGLE hWidget IN WIDGET-POOL cMenuTreePool
        ASSIGN
            FRAME = iphFrame
            NAME = STRING(ipiOrder)
            COL = 1
            ROW = 1
            SENSITIVE = YES
            HIDDEN = YES
            WIDTH = iphFrame:WIDTH - 3
            HEIGHT = ttMenuTree.hImage:HEIGHT + ttMenuTree.hEditor:HEIGHT + .4
            BGCOLOR = 26
            EDGE-CHARS = 0
/*            ROUNDED = YES*/
            PRIVATE-DATA = "," + STRING(ROWID(ttMenuTree))
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClickMenuTree-1 IN THIS-PROCEDURE (hWidget:HANDLE).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN
    ttMenuTree.hRectangle = hWidget.

    IF NOT VALID-HANDLE(hWindow) THEN
    hWindow = CURRENT-WINDOW.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayMenuTree-1 C-Win 
PROCEDURE pDisplayMenuTree-1 :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame  AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcParent AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dCol   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow   AS DECIMAL NO-UNDO INITIAL 1.5.
    DEFINE VARIABLE dWidth AS DECIMAL NO-UNDO.
    DEFINE VARIABLE i      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

    DEFINE BUFFER bttMenuTree FOR ttMenuTree.
    
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    /* make frame large enough for expanding nodes */
    ASSIGN
        iphFrame:VIRTUAL-HEIGHT = 320
        iCount = 0
        i = 0
        .
    FOR EACH bttMenuTree
        :
        IF bttMenuTree.treeParent NE ipcParent THEN
        ASSIGN
            bttMenuTree.hRectangle:HIDDEN = YES
            bttMenuTree.hImage:HIDDEN     = YES
            bttMenuTree.hEditor:HIDDEN    = YES
            .
        ELSE
        iCount = iCount + 1.
    END. /* each bttMenuTree */
    ASSIGN
        menuTitle:WIDTH IN FRAME {&FRAME-NAME} = 1
        menuTitle:COL = 1
        dWidth = IF iCount GT 6 THEN 140 ELSE 71
        {&WINDOW-NAME}:WIDTH = dWidth
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = dWidth
        FRAME {&FRAME-NAME}:WIDTH = dWidth
        iphFrame:VIRTUAL-WIDTH = dWidth
        iphFrame:WIDTH = dWidth
        btnExit:COL = FRAME {&FRAME-NAME}:WIDTH - btnExit:WIDTH
        . 
    
    FOR EACH bttMenuTree
        WHERE bttMenuTree.treeParent EQ ipcParent
        :
        ASSIGN
            i = i + 1
            dCol = IF iCount GT 6 AND i MOD 2 EQ 0 THEN 71 ELSE 2
            .
        ASSIGN
            bttMenuTree.hRectangle:COL    = dCol 
            bttMenuTree.hRectangle:ROW    = dRow
            bttMenuTree.hRectangle:HIDDEN = NO
            .
        IF lMenuImage THEN 
        ASSIGN
            bttMenuTree.hImage:COL    = bttMenuTree.hRectangle:WIDTH / 2
                                      - bttMenuTree.hImage:WIDTH / 2
                                      + bttMenuTree.hRectangle:COL
            bttMenuTree.hImage:ROW    = bttMenuTree.hRectangle:ROW + .28
            bttMenuTree.hImage:HIDDEN = NO
            .
        ASSIGN
            dWidth = FONT-TABLE:GET-TEXT-WIDTH-CHARS (fMnemonic(bttMenuTree.mnemonic) + " "
                   + bttMenuTree.treeText, iFont)
            bttMenuTree.hEditor:WIDTH  = dWidth + 2
            bttMenuTree.hEditor:COL    = bttMenuTree.hRectangle:WIDTH / 2
                                       - bttMenuTree.hEditor:WIDTH / 2
                                       + bttMenuTree.hRectangle:COL
            bttMenuTree.hEditor:ROW    = bttMenuTree.hImage:ROW
                                       + bttMenuTree.hImage:HEIGHT
            bttMenuTree.hEditor:HIDDEN = NO
            dRow = dRow + IF dCol NE 2 OR iCount LE 6 THEN (bttMenuTree.hRectangle:HEIGHT + .4) ELSE 0
            .
    END. /* each bttMenuTree */

    /* adjust frame height for what is visible */
    FIND LAST bttMenuTree
         WHERE bttMenuTree.treeParent EQ ipcParent
         NO-ERROR.
    IF AVAILABLE bttMenuTree THEN DO:
        dRow = bttMenuTree.hRectangle:ROW
             + bttMenuTree.hRectangle:HEIGHT + 1.4
             .    
        ASSIGN
            {&WINDOW-NAME}:HEIGHT = dRow
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = dRow
            FRAME {&FRAME-NAME}:HEIGHT = dRow
            iphFrame:VIRTUAL-HEIGHT = dRow - iphFrame:ROW + 1
            iphFrame:HEIGHT = dRow - iphFrame:ROW + 1
            .
    END. /* if avail */

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetMenuTitle C-Win 
PROCEDURE pSetMenuTitle :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPrgmName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dWidth AS DECIMAL NO-UNDO.

    FIND FIRST prgrms NO-LOCK
         WHERE prgrms.prgmName EQ ipcPrgmName
         NO-ERROR.
    IF AVAILABLE prgrms THEN DO:
        ASSIGN
            cParent   = prgrms.itemParent
            iLevel    = prgrms.menuLevel + 1
            cLevel[1] = SUBSTRING(prgrms.mnemonic,1,1)
            cLevel[2] = SUBSTRING(prgrms.mnemonic,2,1)
            cLevel[3] = SUBSTRING(prgrms.mnemonic,3,1)
            cPrgTitle = (IF cShowMnemonic NE "None" THEN
                        "[" + cLevel[1] + cLevel[2] + cLevel[3] + "] " ELSE "")
                      + prgrms.prgTitle
            menuTitle:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cPrgTitle
            dWidth    = FONT-TABLE:GET-TEXT-WIDTH-CHARS (cPrgTitle, menuTitle:FONT)
            menuTitle:WIDTH  = dWidth + 1
            menuTitle:COL    = FRAME {&FRAME-NAME}:WIDTH / 2 - menuTitle:WIDTH / 2 + 1
            menuTitle:HIDDEN = NO
            .
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

