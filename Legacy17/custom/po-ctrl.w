&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: po-ctrl.w.w

  Description: P/O Control File

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
DEFINE VARIABLE giCurrPo AS INTEGER NO-UNDO.
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME po-ctrl

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-15 RECT-16 fiNextPo Btn_Update ~
Btn_Close 
&Scoped-Define DISPLAYED-FIELDS po-ctrl.rng-po-no[1] po-ctrl.rng-po-no[2] ~
po-ctrl.prcom po-ctrl.pre-printed-forms 
&Scoped-define DISPLAYED-TABLES po-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE po-ctrl
&Scoped-Define DISPLAYED-OBJECTS fiNextPo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 po-ctrl.rng-po-no[1] po-ctrl.rng-po-no[2] ~
po-ctrl.prcom po-ctrl.pre-printed-forms 

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

DEFINE VARIABLE fiNextPo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Next Purchase Order Number" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.67.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME po-ctrl
     fiNextPo AT ROW 1.33 COL 29 COLON-ALIGNED WIDGET-ID 2
     po-ctrl.rng-po-no[1] AT ROW 2.43 COL 29 COLON-ALIGNED
          LABEL "Range of PO Numers"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     po-ctrl.rng-po-no[2] AT ROW 2.43 COL 44 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     po-ctrl.prcom AT ROW 3.62 COL 31
          LABEL "Print Company Name on P/O's"
          VIEW-AS TOGGLE-BOX
          SIZE 33 BY .81
     po-ctrl.pre-printed-forms AT ROW 4.57 COL 31
          LABEL "Use Pre-printed P/O Forms"
          VIEW-AS TOGGLE-BOX
          SIZE 30 BY .81
     Btn_Update AT ROW 6 COL 32 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 6 COL 48 HELP
          "Cancel Update or Close Window"
     RECT-15 AT ROW 5.76 COL 31
     RECT-16 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64.2 BY 6.5.


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
         TITLE              = "P/O Control"
         HEIGHT             = 6.52
         WIDTH              = 64.2
         MAX-HEIGHT         = 6.52
         MAX-WIDTH          = 64.2
         VIRTUAL-HEIGHT     = 6.52
         VIRTUAL-WIDTH      = 64.2
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
/* SETTINGS FOR FRAME po-ctrl
   FRAME-NAME                                                           */
ASSIGN
       Btn_Close:PRIVATE-DATA IN FRAME po-ctrl     = 
                "ribbon-button".


ASSIGN
       Btn_Update:PRIVATE-DATA IN FRAME po-ctrl     = 
                "ribbon-button".


/* SETTINGS FOR TOGGLE-BOX po-ctrl.prcom IN FRAME po-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX po-ctrl.pre-printed-forms IN FRAME po-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN po-ctrl.rng-po-no[1] IN FRAME po-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN po-ctrl.rng-po-no[2] IN FRAME po-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* P/O Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* P/O Control */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME po-ctrl /* Close */
DO:
  IF {&SELF-NAME}:LABEL = "&Close" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:

    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p */
    RUN enable_UI.
    fiNextPo:SENSITIVE = NO.
    RUN sys/ref/asicurseq.p (INPUT gcompany, INPUT "po_seq", OUTPUT giCurrPO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    fiNextPo:SCREEN-VALUE = STRING(giCurrPo  + 1, ">>>>>>").    
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update C-Win
ON CHOOSE OF Btn_Update IN FRAME po-ctrl /* Update */
DO:
  IF {&SELF-NAME}:LABEL = "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
     fiNextPo:SENSITIVE = YES.
    ENABLE {&LIST-1}.
    RUN sys/ref/asicurseq.p (INPUT gcompany, INPUT "po_seq", OUTPUT giCurrPO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    FIND CURRENT po-ctrl EXCLUSIVE-LOCK.
    po-ctrl.next-po-no = giCurrPo + 1.

    fiNextPo:SCREEN-VALUE = STRING(giCurrPO  + 1, ">>>>>>").    
    {methods/setButton.i Btn_Update "Save"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_Close "Cancel"} /* added by script _nonAdm1Images1.p */
    FIND CURRENT po-ctrl no-lock.
    APPLY "ENTRY" TO fiNextPo.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    fiNextPo:SENSITIVE = NO.
    DISABLE {&LIST-1}.
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p */
    FIND CURRENT po-ctrl EXCLUSIVE-LOCK.
    ASSIGN {&LIST-1}.
    DEFINE VARIABLE liNextPo AS INTEGER NO-UNDO.
    FIND company WHERE company.company EQ gcompany NO-LOCK NO-ERROR.
    liNextPo = INTEGER(fiNextPo:SCREEN-VALUE).
    /* Sequence holds the current value. They've entered the next value, so */
    /* subtract 1 to make it the correct current value */
     liNextPo = liNextPo - 1.
     DYNAMIC-CURRENT-VALUE("po_seq" + company.spare-char-1, "ASI") = liNextpo.
     fiNextPo:SCREEN-VALUE = STRING(DYNAMIC-CURRENT-VALUE("po_seq" + company.spare-char-1, "ASI") + 1, ">>>>>>").
     FIND CURRENT po-ctrl EXCLUSIVE-LOCK.
     po-ctrl.next-po-no = INTEGER(fiNextPo:SCREEN-VALUE).    

    FIND CURRENT po-ctrl NO-LOCK.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
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

  IF NOT CAN-FIND(FIRST po-ctrl WHERE po-ctrl.company EQ gcompany) THEN DO:
    CREATE po-ctrl.
    po-ctrl.company = gcompany.
  END.
  FIND FIRST po-ctrl WHERE po-ctrl.company EQ gcompany NO-LOCK NO-ERROR.

  RUN enable_UI.


  {methods/nowait.i}

  APPLY "ENTRY":U TO Btn_Update IN FRAME {&FRAME-NAME}.

  RUN sys/ref/asicurseq.p (INPUT gcompany, INPUT "po_seq", OUTPUT giCurrPo) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

  fiNextPo:SCREEN-VALUE = STRING(giCurrPo  + 1, ">>>>>>").
  fiNextPo:SENSITIVE = NO. 
  DISABLE fiNextPo. 

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
  DISPLAY fiNextPo 
      WITH FRAME po-ctrl IN WINDOW C-Win.
  IF AVAILABLE po-ctrl THEN 
    DISPLAY po-ctrl.rng-po-no[1] po-ctrl.rng-po-no[2] po-ctrl.prcom 
          po-ctrl.pre-printed-forms 
      WITH FRAME po-ctrl IN WINDOW C-Win.
  ENABLE RECT-15 RECT-16 fiNextPo Btn_Update Btn_Close 
      WITH FRAME po-ctrl IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-po-ctrl}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

