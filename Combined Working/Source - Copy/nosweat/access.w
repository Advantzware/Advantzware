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

  Created:           11/27/96

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

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE menu# AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE wk-items NO-UNDO
  FIELD item-name AS CHARACTER
  FIELD item-level AS INTEGER
  FIELD panels AS CHARACTER.

DEFINE BUFFER b-items FOR wk-items.

DEFINE TEMP-TABLE panels NO-UNDO
  FIELD parentName AS CHARACTER
  FIELD panelName AS CHARACTER
  FIELD prgtitle AS CHARACTER
    INDEX panels IS PRIMARY UNIQUE parentName panelName.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 m_menus can_run can_update can_create ~
can_delete Btn_Default Btn_Select Btn_Unselect Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS m_default m_menus can_run can_update ~
can_create can_delete 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPrgmName C-Win 
FUNCTION setPrgmName RETURNS CHARACTER
  (ipPrgmName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "&Cancel" 
     SIZE 12.6 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Default 
     LABEL "Show Current &Users" 
     SIZE 24.6 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_OK DEFAULT 
     LABEL "&Save" 
     SIZE 12.6 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Select 
     LABEL "Select &All" 
     SIZE 12.6 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Unselect 
     LABEL "U&nselect" 
     SIZE 12.6 BY 1.24
     FONT 4.

DEFINE VARIABLE can_create AS CHARACTER INITIAL "*" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67.2 BY 5 NO-UNDO.

DEFINE VARIABLE can_delete AS CHARACTER INITIAL "*" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67.2 BY 5 NO-UNDO.

DEFINE VARIABLE can_run AS CHARACTER INITIAL "*" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67.2 BY 5 NO-UNDO.

DEFINE VARIABLE can_update AS CHARACTER INITIAL "*" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67.2 BY 5 NO-UNDO.

DEFINE VARIABLE m_default AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 1.76.

DEFINE VARIABLE m_menus AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE NO-DRAG SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Loading Menu Structure ...","''" 
     SIZE 82 BY 23.52
     FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     m_default AT ROW 1.24 COL 122 NO-LABELS
     m_menus AT ROW 2.24 COL 1 HELP
          "Select Menu" NO-LABELS
     can_run AT ROW 2.38 COL 99.6 HELP
          "Edit Security Access Values" NO-LABELS
     can_update AT ROW 7.67 COL 99.6 HELP
          "Edit Security Access Values" NO-LABELS
     can_create AT ROW 12.95 COL 99.6 HELP
          "Edit Security Access Values" NO-LABELS
     can_delete AT ROW 18.33 COL 99.6 HELP
          "Edit Security Access Values" NO-LABELS
     Btn_Default AT ROW 24.1 COL 85 HELP
          "Use this function to SET DEFAULT access values"
     Btn_Select AT ROW 24.1 COL 111 HELP
          "Use this function to SELECT ALL Program Items"
     Btn_Unselect AT ROW 24.1 COL 125 HELP
          "Use this function to UNSELECT seletected Program Items"
     Btn_OK AT ROW 24.1 COL 139 HELP
          "Use this function to GENERATE MENU"
     Btn_Cancel AT ROW 24.1 COL 153 HELP
          "Use this function to CANCEL"
     "Can Run" VIEW-AS TEXT
          SIZE 10.6 BY 1.24 AT ROW 2.43 COL 88
          FONT 6
     "Can Create" VIEW-AS TEXT
          SIZE 12.8 BY 1.24 AT ROW 13.14 COL 86
          FONT 6
     "Can Delete" VIEW-AS TEXT
          SIZE 12.8 BY 1.24 AT ROW 18.52 COL 86
          FONT 6
     "Can Update" VIEW-AS TEXT
          SIZE 13.8 BY 1.24 AT ROW 7.67 COL 85
          FONT 6
     "Default Selected:" VIEW-AS TEXT
          SIZE 20.4 BY 1 AT ROW 1.24 COL 101
          FONT 6
     "Menu Structure" VIEW-AS TEXT
          SIZE 18.6 BY 1 AT ROW 1 COL 24
          FONT 6
     RECT-3 AT ROW 23.86 COL 84
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.4 BY 24.76.


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
         MAX-WIDTH          = 175
         VIRTUAL-HEIGHT     = 24.76
         VIRTUAL-WIDTH      = 175
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
                                                                        */
/* SETTINGS FOR FILL-IN m_default IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Security Access by Menu */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
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


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Default
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Default C-Win
ON CHOOSE OF Btn_Default IN FRAME DEFAULT-FRAME /* Show Current Users */
DO:
  DO i = 1 TO m_menus:NUM-ITEMS:
    IF NOT m_menus:IS-SELECTED(i) THEN NEXT.
    FIND FIRST prgrms WHERE prgrms.prgmname EQ m_menus:ENTRY(i) NO-LOCK.
    ASSIGN
      can_run:SCREEN-VALUE = prgrms.can_run
      can_update:SCREEN-VALUE = prgrms.can_update
      can_create:SCREEN-VALUE = prgrms.can_create
      can_delete:SCREEN-VALUE = prgrms.can_delete
      m_default:SCREEN-VALUE = m_menus:ENTRY(i).
    LEAVE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Save */
DO:
  {methods/wait.i}.
  DO i = 1 TO m_menus:NUM-ITEMS:
    IF m_menus:IS-SELECTED(i) THEN DO TRANSACTION:
      FIND FIRST prgrms EXCLUSIVE-LOCK
           WHERE prgrms.prgmname EQ m_menus:ENTRY(i) NO-ERROR.
      IF AVAILABLE prgrms THEN
      ASSIGN
         prgrms.can_run = can_run:SCREEN-VALUE
         prgrms.can_update = can_update:SCREEN-VALUE
         prgrms.can_create = can_create:SCREEN-VALUE
         prgrms.can_delete = can_delete:SCREEN-VALUE.
    END.
  END.
  Btn_Cancel:LABEL = "&Close".
  {methods/nowait.i}.
  MESSAGE "Access Permissions for Highlighted Selections Set" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select C-Win
ON CHOOSE OF Btn_Select IN FRAME DEFAULT-FRAME /* Select All */
DO:
  DO i = 1 TO m_menus:NUM-ITEMS:
    m_menus:SCREEN-VALUE = m_menus:ENTRY(i).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Unselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Unselect C-Win
ON CHOOSE OF Btn_Unselect IN FRAME DEFAULT-FRAME /* Unselect */
DO:
  m_menus:SCREEN-VALUE = "".
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
  END.
  RUN buildPanels.
  RUN enable_UI.
  RUN Enhance IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).
  INPUT FROM value(SEARCH("stdMenu/menu_plus.lst")) NO-ECHO.
  RUN Get_Menus.
  INPUT CLOSE.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildPanels C-Win 
PROCEDURE buildPanels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DEFINE BUFFER bPrgrms FOR prgrms.

  FOR EACH bPrgrms NO-LOCK WHERE bPrgrms.mfgroup NE '':
    DO i = 1 TO NUM-ENTRIES(bPrgrms.mfgroup):
      IF NOT CAN-FIND(FIRST prgrms WHERE prgrms.prgmname EQ ENTRY(i,bPrgrms.mfgroup)) THEN NEXT.
      IF CAN-FIND(panels WHERE panels.parentName EQ ENTRY(i,bPrgrms.mfgroup)
                           AND panels.panelName EQ bPrgrms.prgmname) THEN NEXT.
      CREATE panels.
      ASSIGN
        panels.parentName = ENTRY(i,bPrgrms.mfgroup)
        panels.panelName = bPrgrms.prgmname
        panels.prgtitle = bPrgrms.prgtitle.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY m_default m_menus can_run can_update can_create can_delete 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-3 m_menus can_run can_update can_create can_delete Btn_Default 
         Btn_Select Btn_Unselect Btn_OK Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPanels C-Win 
PROCEDURE getPanels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLevel AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipPrgmName AS CHARACTER NO-UNDO.

  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

  IF NOT CAN-FIND(FIRST panels WHERE panels.parentName EQ ipPrgmName) THEN RETURN.
  FOR EACH panels NO-LOCK WHERE panels.parentName EQ ipPrgmName
      WITH FRAME {&FRAME-NAME}:
    ldummy = m_menus:ADD-LAST(ipLevel + '** Panel: ' + panels.prgtitle +
                              setPrgmName(panels.panelName),panels.panelName).
  END. /* each panels */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Menus C-Win 
PROCEDURE Get_Menus :
/* -----------------------------------------------------------
  Purpose: Load menu names into selection list.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE m_item1 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE m_item2 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE m_level AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      m_menus:LIST-ITEM-PAIRS = ?
      menu# = 0.
    REPEAT:
      IMPORT m_item1 m_item2.
      IF CAN-DO("RULE,SKIP",m_item1) THEN
      NEXT.
      IF INDEX(m_item1,".") EQ 0 THEN DO:
        CREATE wk-items.
        wk-items.item-name = m_item1.
        IF m_item1 NE m_item2 THEN DO:
          FIND FIRST b-items WHERE b-items.item-name EQ m_item2.
          wk-items.item-level = b-items.item-level + 1.
        END.
      END.
      IF m_item1 EQ m_item2 THEN
      NEXT.
      FIND FIRST wk-items WHERE wk-items.item-name EQ m_item2.
      m_level = IF wk-items.item-level EQ 0 THEN ""
                ELSE FILL("- ",wk-items.item-level).
      FIND prgrms NO-LOCK WHERE prgrms.prgmname EQ m_item1 NO-ERROR.
      IF AVAILABLE prgrms THEN DO:
        ldummy = m_menus:ADD-LAST(m_level + prgrms.prgtitle +
                                  setPrgmName(prgrms.prgmname),prgrms.prgmname).
        RUN getPanels (m_level,prgrms.prgmname).
      END.
      ELSE
      IF CAN-DO("rule,skip",m_item1) THEN
      ldummy = m_menus:ADD-LAST(m_level + CAPS(m_item1),m_item1).
    END.
  END.
  
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
  {methods/setfocus.i Btn_Cancel}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPrgmName C-Win 
FUNCTION setPrgmName RETURNS CHARACTER
  (ipPrgmName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  make screen display value unique by placing unique # out of view
    Notes:  
------------------------------------------------------------------------------*/
  menu# = menu# + 1.
  RETURN ' (' + ipPrgmName + ')' + FILL(' ',100) + STRING(menu#,'9999').

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

