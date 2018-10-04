&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: usermenu.w

  Description: User's Menu Structure

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 11/13/96

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL "usermenu".

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE popup-ptr AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE menu-bar-ptr AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE sub-menu-ptr AS WIDGET-HANDLE EXTENT 60 NO-UNDO.
DEFINE VARIABLE menu-item-ptr AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE exit-item-ptr AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE m_item1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE m_item2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE m_item AS CHARACTER NO-UNDO.
DEFINE VARIABLE m_level AS CHARACTER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE wk-ptrs NO-UNDO
  FIELD menu-name AS CHARACTER
  FIELD smenu-ptr AS WIDGET-HANDLE.

DEFINE TEMP-TABLE wk-items NO-UNDO
  FIELD item-name AS CHARACTER
  FIELD item-level AS INTEGER.

DEFINE BUFFER b-items FOR wk-items.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Build menu-items menu-names RECT-1 ~
RECT-3 RECT-5 RECT-2 Btn_Default Btn_Add_Menu program-names Btn_Reset ~
Btn_Save Btn_Cancel Btn_Up Btn_Down Btn_Add_Rule Btn_Add_Skip Btn_Left ~
Btn_Right Btn_Remove Btn_Add_Program 
&Scoped-Define DISPLAYED-OBJECTS menu-items menu-names program-names 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Add_Menu 
     LABEL "Add &Menu Item" 
     SIZE 22.4 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Add_Program 
     LABEL "Add &Program Item" 
     SIZE 22.4 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Add_Rule 
     LABEL "R&ule" 
     SIZE 8.4 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Add_Skip 
     LABEL "S&kip" 
     SIZE 8.4 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Build 
     IMAGE-UP FILE "Graphics/32x32/drop_down_list.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Build" 
     SIZE 8.4 BY 2
     FONT 4.

DEFINE BUTTON Btn_Cancel 
     LABEL "&Cancel" 
     SIZE 14 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Default 
     IMAGE-UP FILE "Graphics/32x32/drop_down_list.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "De&fault" 
     SIZE 8.4 BY 2
     FONT 4.

DEFINE BUTTON Btn_Down 
     LABEL "&Down" 
     SIZE 8.4 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Left 
     LABEL "<<" 
     SIZE 7 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Remove 
     LABEL "&Remove" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Reset 
     IMAGE-UP FILE "Graphics/32x32/drop_down_list.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Rese&t" 
     SIZE 8.4 BY 2
     FONT 4.

DEFINE BUTTON Btn_Right 
     LABEL ">>" 
     SIZE 8.4 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Save 
     IMAGE-UP FILE "Graphics/32x32/drop_down_list.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8.4 BY 2
     FONT 4.

DEFINE BUTTON Btn_Up 
     LABEL "&Up" 
     SIZE 8.4 BY 1.24
     FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 8    
     SIZE 75.6 BY 2.67
     BGCOLOR 4 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8    
     SIZE 49 BY 2.67
     BGCOLOR 3 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 8    
     SIZE 49 BY 2.67
     BGCOLOR 3 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 8    
     SIZE 19.6 BY 23.81
     BGCOLOR 5 .

DEFINE VARIABLE menu-items AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 75.6 BY 21.19
     FONT 0 NO-UNDO.

DEFINE VARIABLE menu-names AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 49 BY 6.38
     FONT 0 NO-UNDO.

DEFINE VARIABLE program-names AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 49 BY 12.14 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Build AT ROW 3.14 COL 134.8 HELP
          "BUILD Menu Bar from Menu Structure Items"
     menu-items AT ROW 1 COL 2.4 HELP
          "Select Menu Structure Item" NO-LABEL
     menu-names AT ROW 1 COL 79.4 HELP
          "Select Available Menu Item" NO-LABEL
     Btn_Default AT ROW 7.52 COL 134.8 HELP
          "Load Menu Structure from System DEFAULT Menu List"
     Btn_Add_Menu AT ROW 8 COL 94.8 HELP
          "Add a Menu Item to Menu Structure"
     program-names AT ROW 10.05 COL 79.4 HELP
          "Select Available Program Item" NO-LABEL
     Btn_Reset AT ROW 11.52 COL 134.8 HELP
          "RESET Menu Structure from Last Saved File"
     Btn_Save AT ROW 15.52 COL 134.8 HELP
          "SAVE Menu Structure"
     Btn_Cancel AT ROW 22.91 COL 132 HELP
          "CANCEL Menu Structure"
     Btn_Up AT ROW 23 COL 5.2 HELP
          "Move Menu Structure Item UP"
     Btn_Down AT ROW 23 COL 13.6 HELP
          "Move Menu Structure Item DOWN"
     Btn_Add_Rule AT ROW 23 COL 23.4 HELP
          "Add RULE Level to Menu Structure"
     Btn_Add_Skip AT ROW 23 COL 31.8 HELP
          "Add SKIP Level to Menu Structure"
     Btn_Left AT ROW 23 COL 41.6 HELP
          "Delete Level from Menu Structure Item"
     Btn_Right AT ROW 23 COL 48.6 HELP
          "Add Level to Menu Structure Item"
     Btn_Remove AT ROW 23 COL 61.2 HELP
          "REMOVE Menu Structure Item"
     Btn_Add_Program AT ROW 23 COL 94.8 HELP
          "Add a Program Item to Menu Structure"
     "De&fault Menu" VIEW-AS TEXT
          SIZE 14 BY 1.05 AT ROW 9.52 COL 132
          BGCOLOR 5 FGCOLOR 15 FONT 4
     "&Build Menu" VIEW-AS TEXT
          SIZE 11.2 BY 1.24 AT ROW 5.24 COL 133.4
          BGCOLOR 5 FGCOLOR 15 FONT 4
     "Rese&t Menu" VIEW-AS TEXT
          SIZE 12.6 BY 1.24 AT ROW 13.52 COL 133.4
          BGCOLOR 5 FGCOLOR 15 FONT 4
     "&Save Menu" VIEW-AS TEXT
          SIZE 11.2 BY 1.24 AT ROW 17.52 COL 133.4
          BGCOLOR 5 FGCOLOR 15 FONT 4
     RECT-1 AT ROW 22.19 COL 2
     RECT-3 AT ROW 22.19 COL 79
     RECT-5 AT ROW 1 COL 129
     RECT-2 AT ROW 7.38 COL 79.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 147.8 BY 24.19.


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
         TITLE              = "User's Menu Structure"
         HEIGHT             = 24.19
         WIDTH              = 149.8
         MAX-HEIGHT         = 25.29
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 25.29
         VIRTUAL-WIDTH      = 160
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User's Menu Structure */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User's Menu Structure */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_Menu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_Menu C-Win
ON CHOOSE OF Btn_Add_Menu IN FRAME DEFAULT-FRAME /* Add Menu Item */
DO:
  RUN Add_Item ("Menu").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_Program
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_Program C-Win
ON CHOOSE OF Btn_Add_Program IN FRAME DEFAULT-FRAME /* Add Program Item */
DO:
  RUN Add_Item ("Program").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_Rule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_Rule C-Win
ON CHOOSE OF Btn_Add_Rule IN FRAME DEFAULT-FRAME /* Rule */
DO:
  RUN Add_Item ("RULE").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_Skip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_Skip C-Win
ON CHOOSE OF Btn_Add_Skip IN FRAME DEFAULT-FRAME /* Skip */
DO:
  RUN Add_Item ("SKIP").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Build
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Build C-Win
ON CHOOSE OF Btn_Build IN FRAME DEFAULT-FRAME /* Build */
DO:
  RUN Check_Menu.
  IF RETURN-VALUE = "ERROR" THEN
  RETURN NO-APPLY.

  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/menu.tmp").
  RUN Save_Menu.
  OUTPUT CLOSE.

  {methods/wait.i}
  DELETE WIDGET-POOL "menubar" NO-ERROR.
  FOR EACH wk-ptrs:
    DELETE wk-ptrs.
  END.
  CREATE WIDGET-POOL "menubar" PERSISTENT.

  CREATE MENU popup-ptr IN WIDGET-POOL "menubar"
    ASSIGN POPUP-ONLY = TRUE.
  CREATE wk-ptrs.
  ASSIGN
    wk-ptrs.menu-name = "popup"
    wk-ptrs.smenu-ptr = popup-ptr.

  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/menu.tmp") NO-ECHO.
  REPEAT:                          
    IMPORT m_item1 m_item2.
    IF CAN-DO("RULE,SKIP",m_item1) OR INDEX(m_item1,".") NE 0 THEN
    NEXT.
    CREATE wk-ptrs.
    wk-ptrs.menu-name = m_item1.
    IF m_item1 NE m_item2 THEN
    NEXT.
    CREATE MENU menu-bar-ptr IN WIDGET-POOL "menubar".
    wk-ptrs.smenu-ptr = menu-bar-ptr.
  END.

  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/menu.tmp") NO-ECHO.
  REPEAT:
    IMPORT m_item1 m_item2.
    IF m_item1 = m_item2 THEN
    NEXT.
    FIND FIRST wk-ptrs WHERE wk-ptrs.menu-name = m_item2.
    IF CAN-DO("RULE,SKIP",m_item1) THEN
    DO:
      CREATE MENU-ITEM menu-item-ptr IN WIDGET-POOL "menubar"
        ASSIGN PARENT = wk-ptrs.smenu-ptr
               SUBTYPE = m_item1
               PRIVATE-DATA = m_item1.
      NEXT.
    END.
    FIND prgrms WHERE prgrms.prgmname = m_item1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE prgrms THEN
    NEXT.
    IF INDEX(m_item1,".") NE 0 THEN
    CREATE MENU-ITEM menu-item-ptr IN WIDGET-POOL "menubar"
      ASSIGN PARENT = wk-ptrs.smenu-ptr
             LABEL = prgrms.prgtitle
             PRIVATE-DATA = m_item1.
    ELSE
    DO:
      CREATE SUB-MENU sub-menu-ptr[1]
        ASSIGN PARENT = wk-ptrs.smenu-ptr
               LABEL = prgrms.prgtitle
               PRIVATE-DATA = m_item1.
      FIND FIRST wk-ptrs WHERE wk-ptrs.menu-name = m_item1.
      wk-ptrs.smenu-ptr = sub-menu-ptr[1].
    END.
  END.
  INPUT CLOSE.

  CREATE MENU-ITEM exit-item-ptr IN WIDGET-POOL "menubar"
    ASSIGN PARENT = menu-bar-ptr:FIRST-CHILD
           LABEL = "E&xit".
  {&WINDOW-NAME}:MENUBAR = menu-bar-ptr:HANDLE.
  {methods/nowait.i}

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  IF SEARCH("users/" + USERID("NOSWEAT") + "/menu.tmp") NE ? THEN
  OS-DELETE VALUE("users/" + USERID("NOSWEAT") + "/menu.tmp").
  APPLY "CLOSE" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Default
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Default C-Win
ON CHOOSE OF Btn_Default IN FRAME DEFAULT-FRAME /* Default */
DO:
  OS-COPY "./menu.lst" VALUE("users/" + USERID("NOSWEAT") + "/menu.lst").
  APPLY "CHOOSE" TO Btn_Reset.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Down C-Win
ON CHOOSE OF Btn_Down IN FRAME DEFAULT-FRAME /* Down */
DO:
  RUN Move_Item ("Down").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Left
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Left C-Win
ON CHOOSE OF Btn_Left IN FRAME DEFAULT-FRAME /* << */
DO:
  RUN Shift_Item ("Left").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME DEFAULT-FRAME /* Remove */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO menu-items:NUM-ITEMS:
      IF menu-items:IS-SELECTED(i) THEN
      LEAVE.
    END.
    ASSIGN
      ldummy = menu-items:DELETE(i)
      i = IF i GT menu-items:NUM-ITEMS THEN menu-items:NUM-ITEMS ELSE i.
    RUN Select_Item (menu-items:ENTRY(i)).
  END.
  APPLY "VALUE-CHANGED" TO menu-items.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Reset C-Win
ON CHOOSE OF Btn_Reset IN FRAME DEFAULT-FRAME /* Reset */
DO:
  {methods/wait.i}
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      menu-names:LIST-ITEMS = ""
      program-names:LIST-ITEMS = ""
      menu-items:LIST-ITEMS = "".
    FOR EACH prgrms WHERE
             prgrms.menu_item AND CAN-DO(prgrms.can_run,USERID("NOSWEAT"))
        NO-LOCK USE-INDEX si-prgtitle:
      ldummy = IF INDEX(prgrms.prgmname,".") = 0 THEN
               menu-names:ADD-LAST(prgrms.prgtitle) ELSE
               program-names:ADD-LAST(prgrms.prgtitle).
    END.
    IF SEARCH("users/" + USERID("NOSWEAT") + "/menu.lst") NE ? THEN
    INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/menu.lst") NO-ECHO.
    ELSE
    INPUT FROM VALUE("menu.lst") NO-ECHO.
    RUN Build_Menu_Items.
    INPUT CLOSE.
    ASSIGN
      Btn_Cancel:LABEL IN FRAME {&FRAME-NAME} = "&Cancel"
      menu-names:SCREEN-VALUE = menu-names:ENTRY(1)
      program-names:SCREEN-VALUE = program-names:ENTRY(1)
      menu-items:SCREEN-VALUE = menu-items:ENTRY(1).
    APPLY "VALUE-CHANGED" TO menu-items.
  END.
  APPLY "CHOOSE" TO Btn_Build.
  {methods/nowait.i}

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Right
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Right C-Win
ON CHOOSE OF Btn_Right IN FRAME DEFAULT-FRAME /* >> */
DO:
  RUN Shift_Item ("Right").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save C-Win
ON CHOOSE OF Btn_Save IN FRAME DEFAULT-FRAME /* Save */
DO:
  RUN Check_Menu.
  IF ERROR-STATUS:ERROR THEN
  RETURN NO-APPLY.
  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/menu.lst").
  RUN Save_Menu.
  OUTPUT CLOSE.
  APPLY "CHOOSE" TO Btn_Build.
  ASSIGN
    Btn_Cancel:LABEL IN FRAME {&FRAME-NAME} = "&Close"
    init_menu = yes.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Up C-Win
ON CHOOSE OF Btn_Up IN FRAME DEFAULT-FRAME /* Up */
DO:
  RUN Move_Item ("Up").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menu-items
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menu-items C-Win
ON VALUE-CHANGED OF menu-items IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME}:
    i = R-INDEX({&SELF-NAME}:SCREEN-VALUE,"- ").
    i = IF i = 0 THEN 1 ELSE i + 2.
    IF i = 1 THEN
    DISABLE Btn_Left Btn_Add_Rule Btn_Add_Skip.
    ELSE
    ENABLE Btn_Left Btn_Add_Rule Btn_Add_Skip.
    IF menu-items:SCREEN-VALUE = menu-items:ENTRY(1) THEN
    DISABLE Btn_Up.
    ELSE
    ENABLE Btn_Up.
    IF menu-items:SCREEN-VALUE = menu-items:ENTRY(menu-items:NUM-ITEMS) THEN
    DISABLE Btn_Down.
    ELSE
    ENABLE Btn_Down.
    IF menu-items:NUM-ITEMS = 0 THEN
    DISABLE Btn_Remove.
    ELSE
    ENABLE Btn_Remove.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menu-names
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menu-names C-Win
ON DEFAULT-ACTION OF menu-names IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO Btn_Add_Menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME program-names
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL program-names C-Win
ON DEFAULT-ACTION OF program-names IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO Btn_Add_Program.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON ESC OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  APPLY "CHOOSE" TO Btn_Reset.
  APPLY "CHOOSE" TO Btn_Build.
  {methods/nowait.i}
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Add_Item C-Win 
PROCEDURE Add_Item :
/* -----------------------------------------------------------
  Purpose: Add Menu/Program Item to Items List   
  Parameters: Item Type
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER item-type AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF menu-items:NUM-ITEMS = 0 THEN
    ASSIGN
      j = 0
      m_level = ""
      i = 1.
    ELSE
    DO i = 1 TO menu-items:NUM-ITEMS:
      IF NOT menu-items:IS-SELECTED(i) THEN
      NEXT.
      ASSIGN
        j = IF R-INDEX(menu-items:ENTRY(i),"- ") = 0 THEN 0
            ELSE (R-INDEX(menu-items:ENTRY(i),"- ") + 1) / 2
        m_level = FILL("- ",j)
        i = i + 1.
      LEAVE.
    END.
    CASE item-type:
      WHEN "Menu" THEN
      ldummy = menu-items:INSERT(m_level + menu-names:SCREEN-VALUE,i).
      WHEN "Program" THEN
      ldummy = menu-items:INSERT(m_level + program-names:SCREEN-VALUE,i).
      OTHERWISE
      ldummy = menu-items:INSERT(m_level + item-type,i).
    END CASE.
    RUN Select_Item (menu-items:ENTRY(i)).
  END.
  APPLY "VALUE-CHANGED" TO menu-items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Build_Menu_Items C-Win 
PROCEDURE Build_Menu_Items :
/*------------------------------------------------------------------------------
  Purpose:     Build Menu Items Selection List.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/wait.i}
  DO WITH FRAME {&FRAME-NAME}:
    REPEAT:
      IMPORT m_item1 m_item2.
      IF INDEX(m_item1,".") = 0 AND NOT CAN-DO("rule,skip",m_item1) THEN
      DO:
        CREATE wk-items.
        wk-items.item-name = m_item1.
        IF m_item1 NE m_item2 THEN
        DO:
          FIND FIRST b-items WHERE b-items.item-name = m_item2.
          wk-items.item-level = b-items.item-level + 1.
        END.
      END.
      IF m_item1 = m_item2 THEN
      NEXT.
      FIND FIRST wk-items WHERE wk-items.item-name = m_item2.
      m_level = IF wk-items.item-level = 0 THEN ""
                ELSE FILL("- ",wk-items.item-level).
      FIND prgrms WHERE prgrms.prgmname = m_item1 NO-LOCK NO-ERROR.
      IF AVAILABLE prgrms THEN
      ldummy = menu-items:ADD-LAST(m_level + prgrms.prgtitle).
      ELSE
      IF CAN-DO("rule,skip",m_item1) THEN
      ldummy = menu-items:ADD-LAST(m_level + CAPS(m_item1)).
    END.
  END.
  {methods/nowait.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check_Menu C-Win 
PROCEDURE Check_Menu :
/*------------------------------------------------------------------------------
  Purpose:     Check Menu Structure List
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  {methods/wait.i}
  m_item2 = "mainmenu".
  DO i = 1 TO menu-items:NUM-ITEMS IN FRAME {&FRAME-NAME} WITH FRAME {&FRAME-NAME}:
    ASSIGN
      j = IF R-INDEX(ENTRY(i,menu-items:LIST-ITEMS),"- ") = 0 THEN 1
          ELSE (R-INDEX(ENTRY(i,menu-items:LIST-ITEMS),"- ") + 1) / 2 + 1
      m_item1 = REPLACE(ENTRY(i,menu-items:LIST-ITEMS),"- ","").
    IF j GT NUM-ENTRIES(m_item2) THEN
    DO:
      menu-items:SCREEN-VALUE = menu-items:ENTRY(i).
      {methods/nowait.i}
      APPLY "VALUE-CHANGED" TO menu-items.
      MESSAGE "An ERROR Exists with the Highlighted Menu Item" VIEW-AS ALERT-BOX.
      RETURN "ERROR".
    END.
    IF CAN-DO("RULE,SKIP",m_item1) THEN
    NEXT.
    FIND FIRST prgrms WHERE prgrms.prgtitle = m_item1 NO-LOCK.
    IF INDEX(prgrms.prgmname,".") = 0 THEN
      IF j LT NUM-ENTRIES(m_item2) THEN
      ENTRY(j + 1,m_item2) = prgrms.prgmname.
      ELSE
      m_item2 = m_item2 + "," + prgrms.prgmname.
  END.
  {methods/nowait.i}
  RETURN.

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
  DISPLAY menu-items menu-names program-names 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Btn_Build menu-items menu-names RECT-1 RECT-3 RECT-5 RECT-2 
         Btn_Default Btn_Add_Menu program-names Btn_Reset Btn_Save Btn_Cancel 
         Btn_Up Btn_Down Btn_Add_Rule Btn_Add_Skip Btn_Left Btn_Right 
         Btn_Remove Btn_Add_Program 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move_Item C-Win 
PROCEDURE Move_Item :
/* -----------------------------------------------------------
  Purpose: Move Menu Item Up or Down
  Parameters: Up/Down
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO menu-items:NUM-ITEMS:
      IF menu-items:IS-SELECTED(i) THEN
      LEAVE.
    END.
    IF move = "Down" THEN
    ASSIGN
      ldummy = menu-items:INSERT(menu-items:SCREEN-VALUE,i + 2)
      ldummy = menu-items:DELETE(i)
      menu-items:SCREEN-VALUE = menu-items:ENTRY(i + 1).
    ELSE
    ASSIGN
      ldummy = menu-items:INSERT(menu-items:SCREEN-VALUE,i - 1)
      ldummy = menu-items:DELETE(i + 1)
      menu-items:SCREEN-VALUE = menu-items:ENTRY(i - 1).
    Btn_Cancel:LABEL = "&Cancel".
  END.
  APPLY "VALUE-CHANGED" TO menu-items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save_Menu C-Win 
PROCEDURE Save_Menu :
/*------------------------------------------------------------------------------
  Purpose:     Save Menu Structure to User's directory.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  {methods/wait.i}
  PUT UNFORMATTED "mainmenu mainmenu" SKIP.
  m_item2 = "mainmenu".
  DO i = 1 TO menu-items:NUM-ITEMS IN FRAME {&FRAME-NAME} WITH FRAME {&FRAME-NAME}:
    ASSIGN
      j = IF R-INDEX(menu-items:ENTRY(i),"- ") = 0 THEN 1
          ELSE (R-INDEX(menu-items:ENTRY(i),"- ") + 1) / 2 + 1
      m_item1 = REPLACE(menu-items:ENTRY(i),"- ","").
    IF CAN-DO("RULE,SKIP",m_item1) THEN
    DO:
      PUT UNFORMATTED m_item1 " " ENTRY(j,m_item2) SKIP.
      NEXT.
    END.
    FIND FIRST prgrms WHERE prgrms.prgtitle = m_item1 NO-LOCK.
    PUT UNFORMATTED prgrms.prgmname " " ENTRY(j,m_item2) SKIP.
    IF INDEX(prgrms.prgmname,".") = 0 THEN
      IF j LT NUM-ENTRIES(m_item2) THEN
      ENTRY(j + 1,m_item2) = prgrms.prgmname.
      ELSE
      m_item2 = m_item2 + "," + prgrms.prgmname.
  END.
  {methods/nowait.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_Item C-Win 
PROCEDURE Select_Item :
/*------------------------------------------------------------------------------
  Purpose:     Make sure the right item is highlighted.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER item-name AS CHARACTER NO-UNDO.

  DO WHILE TRUE WITH FRAME {&FRAME-NAME}:
    j = menu-items:LOOKUP(item-name).
    IF j = i THEN
    LEAVE.
    ldummy = menu-items:REPLACE("*" + item-name,j).
  END.
  ASSIGN
    menu-items:SCREEN-VALUE = menu-items:ENTRY(i)
    Btn_Cancel:LABEL = "&Cancel".
  DO i = 1 TO menu-items:NUM-ITEMS:
    IF SUBSTR(menu-items:ENTRY(i),1,1) = "*" THEN
    ldummy = menu-items:REPLACE(item-name,i).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Shift_Item C-Win 
PROCEDURE Shift_Item :
/* -----------------------------------------------------------
  Purpose: Move Menu Item Left or Right
  Parameters: Left/Right
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.
  DEFINE VARIABLE new-item AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO menu-items:NUM-ITEMS:
      IF menu-items:IS-SELECTED(i) THEN
      LEAVE.
    END.
    new-item = menu-items:ENTRY(i).
    IF move = "Right" THEN
    new-item = "- " + menu-items:ENTRY(i).
    ELSE
    IF SUBSTR(menu-items:ENTRY(i),1,2) = "- " THEN
    new-item = SUBSTR(menu-items:ENTRY(i),3).
    ldummy = menu-items:REPLACE(new-item,i).
    RUN Select_Item (new-item).
  END.
  APPLY "VALUE-CHANGED" TO menu-items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

