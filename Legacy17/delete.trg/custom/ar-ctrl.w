&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar-ctrl.w.w

  Description: A/R Control File

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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-ctrl

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME ar-ctrl.last-inv ~
ar-ctrl.receivables-dscr ar-ctrl.receivables ar-ctrl.sales ~
ar-ctrl.sales-dscr ar-ctrl.cash-act ar-ctrl.cash-act-dscr ar-ctrl.discount ~
ar-ctrl.discount-dscr ar-ctrl.onac ar-ctrl.onac-dscr ar-ctrl.freight ~
ar-ctrl.freight-dscr ar-ctrl.stax ar-ctrl.stax-dscr 
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ar-ctrl SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ar-ctrl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ar-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ar-ctrl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-16 Btn_Update Btn_Close 
&Scoped-Define DISPLAYED-FIELDS ar-ctrl.last-inv ar-ctrl.receivables-dscr ~
ar-ctrl.receivables ar-ctrl.sales ar-ctrl.sales-dscr ar-ctrl.cash-act ~
ar-ctrl.cash-act-dscr ar-ctrl.discount ar-ctrl.discount-dscr ar-ctrl.onac ~
ar-ctrl.onac-dscr ar-ctrl.freight ar-ctrl.freight-dscr ar-ctrl.stax ~
ar-ctrl.stax-dscr 
&Scoped-define DISPLAYED-TABLES ar-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE ar-ctrl


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 ar-ctrl.last-inv ar-ctrl.receivables ar-ctrl.sales ~
ar-ctrl.cash-act ar-ctrl.discount ar-ctrl.onac ar-ctrl.freight ar-ctrl.stax 
&Scoped-define F1 F1 F-2 F-3 F-4 F-5 F-6 F-7 

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

DEFINE VARIABLE F-6 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-7 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
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
     SIZE 102 BY 9.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ar-ctrl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ar-ctrl.last-inv AT ROW 1.24 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     ar-ctrl.receivables-dscr AT ROW 2.43 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 7 FGCOLOR 15 
     ar-ctrl.receivables AT ROW 2.48 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ar-ctrl.sales AT ROW 3.62 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ar-ctrl.sales-dscr AT ROW 3.62 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 7 FGCOLOR 15 
     ar-ctrl.cash-act AT ROW 4.81 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ar-ctrl.cash-act-dscr AT ROW 4.81 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 7 FGCOLOR 15 
     ar-ctrl.discount AT ROW 6 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ar-ctrl.discount-dscr AT ROW 6 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 7 FGCOLOR 15 
     ar-ctrl.onac AT ROW 7.19 COL 24.2 COLON-ALIGNED
          LABEL "Finance Charge Account"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ar-ctrl.onac-dscr AT ROW 7.19 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 7 FGCOLOR 15 
     ar-ctrl.freight AT ROW 8.38 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ar-ctrl.freight-dscr AT ROW 8.38 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 7 FGCOLOR 15 
     ar-ctrl.stax AT ROW 9.57 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ar-ctrl.stax-dscr AT ROW 9.57 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 7 FGCOLOR 15 
     Btn_Update AT ROW 11.24 COL 27 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 11.24 COL 43 HELP
          "Cancel Update or Close Window"
     F1 AT ROW 2.43 COL 53 NO-LABEL
     F-2 AT ROW 3.62 COL 53 NO-LABEL
     F-3 AT ROW 4.81 COL 53 NO-LABEL
     F-4 AT ROW 6 COL 53 NO-LABEL
     F-5 AT ROW 7.19 COL 53 NO-LABEL
     F-6 AT ROW 8.38 COL 53 NO-LABEL
     F-7 AT ROW 9.57 COL 53 NO-LABEL
     RECT-15 AT ROW 11 COL 26
     RECT-16 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.2 BY 12.


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
         TITLE              = "A/R Control"
         HEIGHT             = 12
         WIDTH              = 102.2
         MAX-HEIGHT         = 12
         MAX-WIDTH          = 102.2
         VIRTUAL-HEIGHT     = 12
         VIRTUAL-WIDTH      = 102.2
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

/* SETTINGS FOR FILL-IN ar-ctrl.cash-act IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.cash-act-dscr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-ctrl.discount IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.discount-dscr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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

/* SETTINGS FOR FILL-IN F-6 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-6:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN F-7 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-7:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN ar-ctrl.freight IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.freight-dscr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-ctrl.last-inv IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.onac IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ar-ctrl.onac-dscr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-ctrl.receivables IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.receivables-dscr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-15 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-ctrl.sales IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.sales-dscr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-ctrl.stax IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.stax-dscr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "ASI.ar-ctrl"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* A/R Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/R Control */
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
    APPLY "ENTRY" TO ar-ctrl.last-inv.
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


&Scoped-define SELF-NAME ar-ctrl.cash-act
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.cash-act C-Win
ON ENTRY OF ar-ctrl.cash-act IN FRAME DEFAULT-FRAME /* Cash Account */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.cash-act C-Win
ON LEAVE OF ar-ctrl.cash-act IN FRAME DEFAULT-FRAME /* Cash Account */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.discount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.discount C-Win
ON ENTRY OF ar-ctrl.discount IN FRAME DEFAULT-FRAME /* Discount taken */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.discount C-Win
ON LEAVE OF ar-ctrl.discount IN FRAME DEFAULT-FRAME /* Discount taken */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.freight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.freight C-Win
ON ENTRY OF ar-ctrl.freight IN FRAME DEFAULT-FRAME /* Freight */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.freight C-Win
ON LEAVE OF ar-ctrl.freight IN FRAME DEFAULT-FRAME /* Freight */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.onac
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.onac C-Win
ON ENTRY OF ar-ctrl.onac IN FRAME DEFAULT-FRAME /* Finance Charge Account */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.onac C-Win
ON LEAVE OF ar-ctrl.onac IN FRAME DEFAULT-FRAME /* Finance Charge Account */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.receivables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.receivables C-Win
ON ENTRY OF ar-ctrl.receivables IN FRAME DEFAULT-FRAME /* Accounts Receivable */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.receivables C-Win
ON LEAVE OF ar-ctrl.receivables IN FRAME DEFAULT-FRAME /* Accounts Receivable */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.sales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.sales C-Win
ON ENTRY OF ar-ctrl.sales IN FRAME DEFAULT-FRAME /* Sales Account */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.sales C-Win
ON LEAVE OF ar-ctrl.sales IN FRAME DEFAULT-FRAME /* Sales Account */
DO:
  {custom/actleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.stax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.stax C-Win
ON ENTRY OF ar-ctrl.stax IN FRAME DEFAULT-FRAME /* Sales Tax */
DO:
  {custom/actentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.stax C-Win
ON LEAVE OF ar-ctrl.stax IN FRAME DEFAULT-FRAME /* Sales Tax */
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
  FIND ar-ctrl WHERE ar-ctrl.company = gcompany NO-LOCK NO-ERROR.
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
  IF AVAILABLE ar-ctrl THEN 
    DISPLAY ar-ctrl.last-inv ar-ctrl.receivables-dscr ar-ctrl.receivables 
          ar-ctrl.sales ar-ctrl.sales-dscr ar-ctrl.cash-act 
          ar-ctrl.cash-act-dscr ar-ctrl.discount ar-ctrl.discount-dscr 
          ar-ctrl.onac ar-ctrl.onac-dscr ar-ctrl.freight ar-ctrl.freight-dscr 
          ar-ctrl.stax ar-ctrl.stax-dscr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-16 Btn_Update Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

