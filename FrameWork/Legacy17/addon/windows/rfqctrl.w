&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

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
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rfq-ctrl

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define FIELDS-IN-QUERY-FRAME-A rfq-ctrl.rfq-num ~
rfq-ctrl.rfq-range[1] rfq-ctrl.rfq-range[2] rfq-ctrl.def-cas-w ~
rfq-ctrl.def-case rfq-ctrl.def-coat rfq-ctrl.def-ink rfq-ctrl.def-inkcov ~
rfq-ctrl.def-pal rfq-ctrl.def-pal-w rfq-ctrl.avg-cscost rfq-ctrl.rm-rate ~
rfq-ctrl.avg-cswt rfq-ctrl.sell-by rfq-ctrl.avg-palcost rfq-ctrl.sho-labor ~
rfq-ctrl.avg-palwt rfq-ctrl.shp-add rfq-ctrl.comm-add rfq-ctrl.shp-mrkup ~
rfq-ctrl.comm-calc rfq-ctrl.comm-mrkup rfq-ctrl.spec-pct 
&Scoped-define QUERY-STRING-FRAME-A FOR EACH rfq-ctrl SHARE-LOCK
&Scoped-define OPEN-QUERY-FRAME-A OPEN QUERY FRAME-A FOR EACH rfq-ctrl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-A rfq-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-A rfq-ctrl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_update btn_close RECT-24 RECT-25 RECT-26 
&Scoped-Define DISPLAYED-FIELDS rfq-ctrl.rfq-num rfq-ctrl.rfq-range[1] ~
rfq-ctrl.rfq-range[2] rfq-ctrl.def-cas-w rfq-ctrl.def-case ~
rfq-ctrl.def-coat rfq-ctrl.def-ink rfq-ctrl.def-inkcov rfq-ctrl.def-pal ~
rfq-ctrl.def-pal-w rfq-ctrl.avg-cscost rfq-ctrl.rm-rate rfq-ctrl.avg-cswt ~
rfq-ctrl.sell-by rfq-ctrl.avg-palcost rfq-ctrl.sho-labor rfq-ctrl.avg-palwt ~
rfq-ctrl.shp-add rfq-ctrl.comm-add rfq-ctrl.shp-mrkup rfq-ctrl.comm-calc ~
rfq-ctrl.comm-mrkup rfq-ctrl.spec-pct 
&Scoped-define DISPLAYED-TABLES rfq-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE rfq-ctrl


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 rfq-ctrl.rfq-num rfq-ctrl.rfq-range[1] ~
rfq-ctrl.rfq-range[2] rfq-ctrl.def-cas-w rfq-ctrl.def-case ~
rfq-ctrl.def-coat rfq-ctrl.def-ink rfq-ctrl.def-inkcov rfq-ctrl.def-pal ~
rfq-ctrl.def-pal-w rfq-ctrl.avg-cscost rfq-ctrl.rm-rate rfq-ctrl.avg-cswt ~
rfq-ctrl.sell-by rfq-ctrl.avg-palcost rfq-ctrl.sho-labor rfq-ctrl.avg-palwt ~
rfq-ctrl.shp-add rfq-ctrl.comm-add rfq-ctrl.shp-mrkup rfq-ctrl.comm-calc ~
rfq-ctrl.comm-mrkup rfq-ctrl.spec-pct 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_close 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91.8 BY 1.67.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 7.62.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 9.52.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FRAME-A FOR 
      rfq-ctrl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     rfq-ctrl.rfq-num AT ROW 1.48 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.rfq-range[1] AT ROW 1.48 COL 46 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.rfq-range[2] AT ROW 1.48 COL 78 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.def-cas-w AT ROW 4.1 COL 29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.def-case AT ROW 5.05 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfq-ctrl.def-coat AT ROW 6.1 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfq-ctrl.def-ink AT ROW 7.19 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfq-ctrl.def-inkcov AT ROW 8.14 COL 33 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfq-ctrl.def-pal AT ROW 9.1 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfq-ctrl.def-pal-w AT ROW 10.1 COL 29.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.avg-cscost AT ROW 12.43 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     rfq-ctrl.rm-rate AT ROW 12.67 COL 31 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.avg-cswt AT ROW 13.43 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     rfq-ctrl.sell-by AT ROW 13.67 COL 37.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     rfq-ctrl.avg-palcost AT ROW 14.43 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     rfq-ctrl.sho-labor AT ROW 14.67 COL 32.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     rfq-ctrl.avg-palwt AT ROW 15.43 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.shp-add AT ROW 15.67 COL 37.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     rfq-ctrl.comm-add AT ROW 16.43 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     rfq-ctrl.shp-mrkup AT ROW 16.67 COL 31 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.comm-calc AT ROW 17.43 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     rfq-ctrl.comm-mrkup AT ROW 18.43 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.spec-pct AT ROW 19.33 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     btn_update AT ROW 22.19 COL 56
     btn_close AT ROW 22.19 COL 78
     RECT-24 AT ROW 1.24 COL 4
     RECT-25 AT ROW 3.86 COL 4
     RECT-26 AT ROW 12.19 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 1
         SIZE 99.8 BY 22.67.


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
         TITLE              = "Request For Quote Control"
         HEIGHT             = 22.67
         WIDTH              = 100.2
         MAX-HEIGHT         = 22.67
         MAX-WIDTH          = 100.2
         VIRTUAL-HEIGHT     = 22.67
         VIRTUAL-WIDTH      = 100.2
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
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* SETTINGS FOR FILL-IN rfq-ctrl.avg-cscost IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.avg-cswt IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.avg-palcost IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.avg-palwt IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.comm-add IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.comm-calc IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.comm-mrkup IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.def-cas-w IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.def-case IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.def-coat IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.def-ink IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.def-inkcov IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.def-pal IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.def-pal-w IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.rfq-num IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.rfq-range[1] IN FRAME FRAME-A
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN rfq-ctrl.rfq-range[2] IN FRAME FRAME-A
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN rfq-ctrl.rm-rate IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.sell-by IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.sho-labor IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.shp-add IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.shp-mrkup IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN rfq-ctrl.spec-pct IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _TblList          = "rfq.rfq-ctrl"
     _Query            is OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Request For Quote Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Request For Quote Control */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_close C-Win
ON CHOOSE OF btn_close IN FRAME FRAME-A /* Close */
DO:
  IF {&SELF-NAME}:LABEL = "&Close" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Close"
      Btn_Update:LABEL = "&Update".
    RUN enable_UI.
  END.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_update C-Win
ON CHOOSE OF btn_update IN FRAME FRAME-A /* Update */
DO:
  IF {&SELF-NAME}:LABEL = "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&LIST-1}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Save"
      Btn_Close:LABEL = "&Cancel".
    APPLY "ENTRY" TO rfq-ctrl.rfq-num.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Update"
      Btn_Close:LABEL = "&Close".
    ASSIGN {&LIST-1}.
  END.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
  FIND rfq-ctrl WHERE rfq-ctrl.company = gcompany
                 AND rfq-ctrl.loc = gloc NO-LOCK NO-ERROR.
  RUN enable_UI.

  {methods/nowait.i}
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

  {&OPEN-QUERY-FRAME-A}
  GET FIRST FRAME-A.
  IF AVAILABLE rfq-ctrl THEN 
    DISPLAY rfq-ctrl.rfq-num rfq-ctrl.rfq-range[1] rfq-ctrl.rfq-range[2] 
          rfq-ctrl.def-cas-w rfq-ctrl.def-case rfq-ctrl.def-coat 
          rfq-ctrl.def-ink rfq-ctrl.def-inkcov rfq-ctrl.def-pal 
          rfq-ctrl.def-pal-w rfq-ctrl.avg-cscost rfq-ctrl.rm-rate 
          rfq-ctrl.avg-cswt rfq-ctrl.sell-by rfq-ctrl.avg-palcost 
          rfq-ctrl.sho-labor rfq-ctrl.avg-palwt rfq-ctrl.shp-add 
          rfq-ctrl.comm-add rfq-ctrl.shp-mrkup rfq-ctrl.comm-calc 
          rfq-ctrl.comm-mrkup rfq-ctrl.spec-pct 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE btn_update btn_close RECT-24 RECT-25 RECT-26 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

