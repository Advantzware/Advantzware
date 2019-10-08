&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: gl-ctrl.w.w

  Description: G/L Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{custom/format.i}

{sys/inc/VAR.i  NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Update Btn_Close RECT-15 RECT-16 
&Scoped-Define DISPLAYED-FIELDS gl-ctrl.journal gl-ctrl.trnum gl-ctrl.ret ~
gl-ctrl.ret-dscr gl-ctrl.contra gl-ctrl.con-dscr 
&Scoped-define DISPLAYED-TABLES gl-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE gl-ctrl


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 gl-ctrl.journal gl-ctrl.ret gl-ctrl.contra 
&Scoped-define List-2 gl-ctrl.ret-dscr gl-ctrl.con-dscr 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 1.67.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 93 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     gl-ctrl.journal AT ROW 1.24 COL 26 COLON-ALIGNED
          LABEL "Last Journal Number"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     gl-ctrl.trnum AT ROW 2.43 COL 26 COLON-ALIGNED
          LABEL "Last Transaction Number"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     gl-ctrl.ret AT ROW 3.62 COL 26 COLON-ALIGNED
          LABEL "Current Year Earnings"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     gl-ctrl.ret-dscr AT ROW 3.62 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          BGCOLOR 7 FGCOLOR 15 
     gl-ctrl.contra AT ROW 4.81 COL 26 COLON-ALIGNED
          LABEL "Profit Contra"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     gl-ctrl.con-dscr AT ROW 4.81 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          BGCOLOR 7 FGCOLOR 15 
     Btn_Update AT ROW 6.48 COL 28 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 6.48 COL 44 HELP
          "Cancel Update or Close Window"
     RECT-15 AT ROW 6.24 COL 27
     RECT-16 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93.2 BY 10.05.


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
         TITLE              = "G/L Control"
         HEIGHT             = 10.05
         WIDTH              = 93.2
         MAX-HEIGHT         = 10.05
         MAX-WIDTH          = 93.2
         VIRTUAL-HEIGHT     = 10.05
         VIRTUAL-WIDTH      = 93.2
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
                                                                        */
ASSIGN
       Btn_Close:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       Btn_Update:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN gl-ctrl.con-dscr IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN gl-ctrl.contra IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN gl-ctrl.journal IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN gl-ctrl.ret IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN gl-ctrl.ret-dscr IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN gl-ctrl.trnum IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
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
ON END-ERROR OF C-Win /* G/L Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* G/L Control */
DO:
/* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* Close */
DO:
  IF {&SELF-NAME}:LABEL = "&Close" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p */
    RUN enable_UI.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update C-Win
ON CHOOSE OF Btn_Update IN FRAME DEFAULT-FRAME /* Update */
DO:
  IF {&SELF-NAME}:LABEL = "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&LIST-1}.
    DISPLAY {&F1}.
    {methods/setButton.i Btn_Update "Save"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_Close "Cancel"} /* added by script _nonAdm1Images1.p */
    APPLY "ENTRY" TO gl-ctrl.journal.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-glacct ("ret")    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-glacct ("contra") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    DISABLE {&LIST-1}.
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p */
    FIND CURRENT gl-ctrl EXCLUSIVE-LOCK.
    ASSIGN {&LIST-1} {&LIST-2}.
    FIND CURRENT gl-ctrl NO-LOCK.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-ctrl.contra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-ctrl.contra C-Win
ON LEAVE OF gl-ctrl.contra IN FRAME DEFAULT-FRAME /* Profit Contra */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-glacct ("contra") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-ctrl.contra C-Win
ON VALUE-CHANGED OF gl-ctrl.contra IN FRAME DEFAULT-FRAME /* Profit Contra */
DO:
  FIND account
      WHERE account.company EQ cocode
        AND account.actnum  BEGINS {&self-name}:SCREEN-VALUE
        AND account.TYPE    EQ "E"
      NO-LOCK NO-ERROR.
  IF AVAIL account THEN DO:
    {&self-name}:SCREEN-VALUE = account.actnum.
    APPLY "tab" TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-ctrl.journal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-ctrl.journal C-Win
ON ENTRY OF gl-ctrl.journal IN FRAME DEFAULT-FRAME /* Last Journal Number */
DO:
      APPLY "tab" TO {&self-name}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-ctrl.ret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-ctrl.ret C-Win
ON LEAVE OF gl-ctrl.ret IN FRAME DEFAULT-FRAME /* Current Year Earnings */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-glacct ("ret") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-ctrl.ret C-Win
ON VALUE-CHANGED OF gl-ctrl.ret IN FRAME DEFAULT-FRAME /* Current Year Earnings */
DO:
  FIND account
      WHERE account.company EQ cocode
        AND account.actnum  BEGINS {&self-name}:SCREEN-VALUE
        AND account.TYPE    EQ "C"
      NO-LOCK NO-ERROR.
  IF AVAIL account THEN DO:
    {&self-name}:SCREEN-VALUE = account.actnum.
    APPLY "tab" TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN.
  END.

  IF NOT CAN-FIND(FIRST gl-ctrl WHERE gl-ctrl.company EQ gcompany) THEN DO:
    CREATE gl-ctrl.
    gl-ctrl.company = gcompany.
  END.
  FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ gcompany NO-LOCK NO-ERROR.

  RUN enable_UI.

  {methods/nowait.i}

    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  IF AVAILABLE gl-ctrl THEN 
    DISPLAY gl-ctrl.journal gl-ctrl.trnum gl-ctrl.ret gl-ctrl.ret-dscr 
          gl-ctrl.contra gl-ctrl.con-dscr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Btn_Update Btn_Close RECT-15 RECT-16 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-glacct C-Win 
PROCEDURE valid-glacct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER ip-field LIKE account.TYPE NO-UNDO.

  DEF VAR lv-acct LIKE account.actnum NO-UNDO.
  DEF VAR lv-type AS CHAR NO-UNDO.
  DEF VAR lv-types AS CHAR INIT "ACELRT" NO-UNDO.
  DEF VAR lv-type-dscr AS CHAR INIT
      "Asset,Capital,Expense,Liability,Revenue,Total" NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF ip-field EQ "ret" THEN
      ASSIGN
       lv-acct = gl-ctrl.ret:SCREEN-VALUE
       lv-type = "C".
    ELSE
    IF ip-field EQ "contra" THEN
      ASSIGN
       lv-acct = gl-ctrl.contra:SCREEN-VALUE
       lv-type = "E".

    FIND FIRST account
        WHERE account.company EQ cocode
          AND account.actnum  EQ lv-acct
          AND account.TYPE    EQ lv-type
        NO-LOCK NO-ERROR.
    IF AVAIL account THEN DO:
      IF ip-field EQ "ret"    THEN gl-ctrl.ret-dscr:SCREEN-VALUE = account.dscr.
      ELSE
      IF ip-field EQ "contra" THEN gl-ctrl.con-dscr:SCREEN-VALUE = account.dscr.
    END.

    ELSE DO:
      MESSAGE "Invalid " +
              TRIM(ENTRY(INDEX(lv-types,lv-type),lv-type-dscr)) +
              " account#, try help..."
              VIEW-AS ALERT-BOX ERROR.
      IF ip-field EQ "ret"    THEN APPLY "entry" TO gl-ctrl.ret.
      ELSE
      IF ip-field EQ "contra" THEN APPLY "entry" TO gl-ctrl.contra.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

