&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap-ctrl.w.w

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
{custom/format.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ap-ctrl

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME ap-ctrl.payables ~
ap-ctrl.purchases ap-ctrl.cash-act ap-ctrl.discount ap-ctrl.stax 
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ap-ctrl SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ap-ctrl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ap-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ap-ctrl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-16 Btn_Update Btn_Close 
&Scoped-Define DISPLAYED-FIELDS ap-ctrl.payables ap-ctrl.purchases ~
ap-ctrl.cash-act ap-ctrl.discount ap-ctrl.stax 
&Scoped-define DISPLAYED-TABLES ap-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE ap-ctrl


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 ap-ctrl.payables ap-ctrl.purchases ap-ctrl.cash-act ~
ap-ctrl.discount ap-ctrl.stax 
&Scoped-define F1 F1 F-2 F-3 F-4 F-5 

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

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-4 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-5 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.67.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 6.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ap-ctrl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ap-ctrl.payables AT ROW 1.24 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ap-ctrl.purchases AT ROW 2.43 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ap-ctrl.cash-act AT ROW 3.62 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ap-ctrl.discount AT ROW 4.81 COL 20 COLON-ALIGNED
          LABEL "Discount Taken"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ap-ctrl.stax AT ROW 6 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     Btn_Update AT ROW 7.67 COL 20 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 7.67 COL 36 HELP
          "Cancel Update or Close Window"
     F1 AT ROW 1.24 COL 49 NO-LABEL
     F-2 AT ROW 2.43 COL 49 NO-LABEL
     F-3 AT ROW 3.62 COL 49 NO-LABEL
     F-4 AT ROW 4.81 COL 49 NO-LABEL
     F-5 AT ROW 6 COL 49 NO-LABEL
     RECT-16 AT ROW 1 COL 1
     RECT-15 AT ROW 7.43 COL 19
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 51.2 BY 8.19.


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
         TITLE              = "A/P Control"
         HEIGHT             = 8.19
         WIDTH              = 51.2
         MAX-HEIGHT         = 8.19
         MAX-WIDTH          = 51.2
         VIRTUAL-HEIGHT     = 8.19
         VIRTUAL-WIDTH      = 51.2
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
   FRAME-NAME                                                           */
ASSIGN 
       Btn_Close:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       Btn_Update:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN ap-ctrl.cash-act IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ap-ctrl.discount IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN F-2 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN F-3 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN F-4 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN F-5 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-5:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN ap-ctrl.payables IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ap-ctrl.purchases IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RECTANGLE RECT-15 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ap-ctrl.stax IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "ASI.ap-ctrl"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* A/P Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/P Control */
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
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    RUN enable_UI.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
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
    {methods/setButton.i Btn_Update "Save"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Close "Cancel"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    APPLY "ENTRY" TO ap-ctrl.payables.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1}.
    HIDE {&F1} NO-PAUSE.
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    ASSIGN {&LIST-1}.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.cash-act
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.cash-act C-Win
ON ENTRY OF ap-ctrl.cash-act IN FRAME DEFAULT-FRAME /* Cash Account */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.cash-act C-Win
ON LEAVE OF ap-ctrl.cash-act IN FRAME DEFAULT-FRAME /* Cash Account */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.discount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.discount C-Win
ON ENTRY OF ap-ctrl.discount IN FRAME DEFAULT-FRAME /* Discount Taken */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.discount C-Win
ON LEAVE OF ap-ctrl.discount IN FRAME DEFAULT-FRAME /* Discount Taken */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.payables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.payables C-Win
ON ENTRY OF ap-ctrl.payables IN FRAME DEFAULT-FRAME /* Accounts Payable */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.payables C-Win
ON LEAVE OF ap-ctrl.payables IN FRAME DEFAULT-FRAME /* Accounts Payable */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.purchases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.purchases C-Win
ON ENTRY OF ap-ctrl.purchases IN FRAME DEFAULT-FRAME /* Purchases Account */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.purchases C-Win
ON LEAVE OF ap-ctrl.purchases IN FRAME DEFAULT-FRAME /* Purchases Account */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.stax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.stax C-Win
ON ENTRY OF ap-ctrl.stax IN FRAME DEFAULT-FRAME /* Sales Tax */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.stax C-Win
ON LEAVE OF ap-ctrl.stax IN FRAME DEFAULT-FRAME /* Sales Tax */
DO:
  {custom/actleave.i}
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
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND ap-ctrl WHERE ap-ctrl.company = gcompany NO-LOCK NO-ERROR.
  RUN enable_UI.
  {methods/nowait.i}
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE ap-ctrl THEN 
    DISPLAY ap-ctrl.payables ap-ctrl.purchases ap-ctrl.cash-act ap-ctrl.discount 
          ap-ctrl.stax 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-16 Btn_Update Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

