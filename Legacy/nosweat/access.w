&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:              access.w

  Description:       Security Access by Menu

  Input Parameters:  <none>

  Output Parameters: <none>

  Author:            Ron Stark

  Created:           11.27.1996
  Updated:           09.16.2018

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE idx   AS INTEGER NO-UNDO.
DEFINE VARIABLE iMenu AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttPanels NO-UNDO
    FIELD parentName AS CHARACTER
    FIELD panelName  AS CHARACTER
    FIELD prgtitle   AS CHARACTER
        INDEX panels IS PRIMARY UNIQUE
            parentName
            panelName
            .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel btnOK cMenus cCanRun cCanUpdate ~
cCanCreate cCanDelete btnDefault btnSelect btnUnselect 
&Scoped-Define DISPLAYED-OBJECTS cDefault cMenus cCanRun cCanUpdate ~
cCanCreate cCanDelete 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetPrgmName C-Win 
FUNCTION fSetPrgmName RETURNS CHARACTER
    (ipcPrgmName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY DEFAULT 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Cancel" 
     SIZE 7.6 BY 1.81
     BGCOLOR 8 FONT 4.

DEFINE BUTTON btnDefault 
     LABEL "Show Current &Users" 
     SIZE 22.6 BY 1.24
     FONT 4.

DEFINE BUTTON btnOK DEFAULT 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 7.6 BY 1.81
     BGCOLOR 8 FONT 4.

DEFINE BUTTON btnSelect 
     LABEL "Select &All" 
     SIZE 10.6 BY 1.24
     FONT 4.

DEFINE BUTTON btnUnselect 
     LABEL "U&nselect" 
     SIZE 10.6 BY 1.24
     FONT 4.

DEFINE VARIABLE cCanCreate AS CHARACTER INITIAL "*" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67.2 BY 5
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cCanDelete AS CHARACTER INITIAL "*" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67.2 BY 5
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cCanRun AS CHARACTER INITIAL "*" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67.2 BY 5
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cCanUpdate AS CHARACTER INITIAL "*" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67.2 BY 5
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cDefault AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 67 BY 2.24.

DEFINE VARIABLE cMenus AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE NO-DRAG SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Loading Menu Structure ...","''" 
     SIZE 82 BY 23.52
     BGCOLOR 15 FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCancel AT ROW 23.62 COL 158 HELP
          "Use this function to CANCEL"
     btnOK AT ROW 23.62 COL 150 HELP
          "Use this function to GENERATE MENU"
     cDefault AT ROW 1.24 COL 121 NO-LABEL
     cMenus AT ROW 2.24 COL 1 HELP
          "Select Menu" NO-LABEL
     cCanRun AT ROW 2.43 COL 100 HELP
          "Edit Security Access Values" NO-LABEL
     cCanUpdate AT ROW 7.67 COL 100 HELP
          "Edit Security Access Values" NO-LABEL
     cCanCreate AT ROW 12.91 COL 100 HELP
          "Edit Security Access Values" NO-LABEL
     cCanDelete AT ROW 18.14 COL 100 HELP
          "Edit Security Access Values" NO-LABEL
     btnDefault AT ROW 23.86 COL 102 HELP
          "Use this function to SET DEFAULT access values"
     btnSelect AT ROW 23.86 COL 126 HELP
          "Use this function to SELECT ALL Program Items"
     btnUnselect AT ROW 23.86 COL 138 HELP
          "Use this function to UNSELECT seletected Program Items"
     "Can Run" VIEW-AS TEXT
          SIZE 10.6 BY 1.24 AT ROW 2.48 COL 88
          FONT 6
     "Can Create" VIEW-AS TEXT
          SIZE 12.8 BY 1.24 AT ROW 13.19 COL 86
          FONT 6
     "Can Delete" VIEW-AS TEXT
          SIZE 12.8 BY 1.24 AT ROW 18.33 COL 86
          FONT 6
     "Can Update" VIEW-AS TEXT
          SIZE 13.8 BY 1.24 AT ROW 7.71 COL 85
          FONT 6
     "Default Selected:" VIEW-AS TEXT
          SIZE 20.4 BY 1 AT ROW 1.24 COL 100
          FONT 6
     "Menu Structure" VIEW-AS TEXT
          SIZE 18.6 BY .81 AT ROW 1.24 COL 2
          FONT 6
     RECT-3 AT ROW 23.38 COL 100
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.4 BY 24.76
         FGCOLOR 1 .


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
         TITLE              = "Security Access by Menu"
         HEIGHT             = 24.76
         WIDTH              = 166.4
         MAX-HEIGHT         = 24.76
         MAX-WIDTH          = 166.4
         VIRTUAL-HEIGHT     = 24.76
         VIRTUAL-WIDTH      = 166.4
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN cDefault IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Security Access by Menu */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
DO:
    /* This case occurs when the user presses the "Esc" key.
       In a persistently run window, just ignore this.  If we did not, the
       application would exit. */
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Security Access by Menu */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefault C-Win
ON CHOOSE OF btnDefault IN FRAME DEFAULT-FRAME /* Show Current Users */
DO:
    DO idx = 1 TO cMenus:NUM-ITEMS:
        IF NOT cMenus:IS-SELECTED(idx) THEN NEXT.
        FIND FIRST prgrms NO-LOCK
             WHERE prgrms.prgmname EQ cMenus:ENTRY(idx)
             NO-ERROR.
        IF AVAILABLE prgrms THEN 
        ASSIGN
            cCanRun:SCREEN-VALUE    = prgrms.can_run
            cCanUpdate:SCREEN-VALUE = prgrms.can_update
            cCanCreate:SCREEN-VALUE = prgrms.can_create
            cCanDelete:SCREEN-VALUE = prgrms.can_delete
            cDefault:SCREEN-VALUE   = cMenus:ENTRY(idx)
            .
        LEAVE.
    END. /* do idx */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME /* Save */
DO:
    {methods/wait.i}.
    DO idx = 1 TO cMenus:NUM-ITEMS:
        IF cMenus:IS-SELECTED(idx) THEN 
        DO TRANSACTION:
            FIND FIRST prgrms EXCLUSIVE-LOCK
                 WHERE prgrms.prgmname EQ cMenus:ENTRY(idx)
                 NO-ERROR.
            IF AVAILABLE prgrms THEN
            ASSIGN
                prgrms.can_run    = cCanRun:SCREEN-VALUE
                prgrms.can_update = cCanUpdate:SCREEN-VALUE
                prgrms.can_create = cCanCreate:SCREEN-VALUE
                prgrms.can_delete = cCanDelete:SCREEN-VALUE
                .
        END. /* do trans */
    END. /* do idx */
    btnCancel:LABEL = "&Close".
    {methods/nowait.i}.
    MESSAGE "Access Permissions for Highlighted Selections Set" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelect C-Win
ON CHOOSE OF btnSelect IN FRAME DEFAULT-FRAME /* Select All */
DO:
    DO idx = 1 TO cMenus:NUM-ITEMS:
        cMenus:SCREEN-VALUE = cMenus:ENTRY(idx).
    END. /* do idx */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUnselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUnselect C-Win
ON CHOOSE OF btnUnselect IN FRAME DEFAULT-FRAME /* Unselect */
DO:
    cMenus:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{SYS/INC/F3HELPW.I}
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
    IF access-close THEN DO:
        APPLY "CLOSE" TO THIS-PROCEDURE.
        RETURN.
    END. /* if access-close */
    RUN pBuildPanels.
    RUN enable_UI.
    RUN Enhance IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).
    INPUT FROM value(SEARCH("stdMenu/menu_plus.lst")) NO-ECHO.
    RUN pGetMenus.
    INPUT CLOSE.
    {methods/nowait.i}
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
  DISPLAY cDefault cMenus cCanRun cCanUpdate cCanCreate cCanDelete 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnCancel btnOK cMenus cCanRun cCanUpdate cCanCreate cCanDelete 
         btnDefault btnSelect btnUnselect 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildPanels C-Win 
PROCEDURE pBuildPanels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DEFINE BUFFER bPrgrms FOR prgrms.

    FOR EACH bPrgrms NO-LOCK
        WHERE bPrgrms.mfgroup NE ''
        :
        DO idx = 1 TO NUM-ENTRIES(bPrgrms.mfgroup):
            IF NOT CAN-FIND(FIRST prgrms
                            WHERE prgrms.prgmname EQ ENTRY(idx,bPrgrms.mfgroup)) THEN
            NEXT.
            IF CAN-FIND(FIRST ttPanels
                        WHERE ttPanels.parentName EQ ENTRY(idx,bPrgrms.mfgroup)
                          AND ttPanels.panelName  EQ bPrgrms.prgmname) THEN
            NEXT.
            CREATE ttPanels.
            ASSIGN
                ttPanels.parentName = ENTRY(idx,bPrgrms.mfgroup)
                ttPanels.panelName  = bPrgrms.prgmname
                ttPanels.prgtitle   = bPrgrms.prgtitle
                .
        END. /* do idx */
    END. /* each prgrms */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetMenus C-Win 
PROCEDURE pGetMenus :
/* -----------------------------------------------------------
  Purpose: Load menu names into selection list.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE VARIABLE cLevel AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cMenus:LIST-ITEM-PAIRS = ?
            iMenu                  = 0
            .
        FOR EACH prgrms NO-LOCK
            WHERE prgrms.menu_item EQ YES
              AND prgrms.menuOrder GT 0
              AND prgrms.menuLevel GT 0
              AND prgrms.mnemonic  NE ""
            :
            cLevel = FILL("- ",prgrms.menuLevel).
            cMenus:ADD-LAST(cLevel + prgrms.prgtitle
                          + fSetPrgmName(prgrms.prgmname),prgrms.prgmname)
                          .
            RUN pGetPanels (cLevel,prgrms.prgmname).
        END. /* each prgrams */
    END. /* with frame */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetPanels C-Win 
PROCEDURE pGetPanels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLevel    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName AS CHARACTER NO-UNDO.

    IF NOT CAN-FIND(FIRST ttPanels
                    WHERE ttPanels.parentName EQ ipcPrgmName) THEN
    RETURN.
    FOR EACH ttPanels NO-LOCK
        WHERE ttPanels.parentName EQ ipcPrgmName
        WITH FRAME {&FRAME-NAME}:
        cMenus:ADD-LAST(ipcLevel + '** Panel: ' + ttPanels.prgtitle
            + fSetPrgmName(ttPanels.panelName),ttPanels.panelName).
    END. /* each ttPanels */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus C-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus done by calling program
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/setfocus.i btnCancel}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetPrgmName C-Win 
FUNCTION fSetPrgmName RETURNS CHARACTER
    (ipcPrgmName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  make screen display value unique by placing unique # out of view
    Notes:  
------------------------------------------------------------------------------*/
    iMenu = iMenu + 1.
    RETURN ' (' + ipcPrgmName + ')' + FILL(' ',100) + STRING(iMenu,'9999').

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

