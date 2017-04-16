&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: PurgeMU2.w

  Description: Utility to purge unwanted MU2 receipts by date

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Brad Vigrass

  Created: 03/01/2013

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
/* {methods/prgsecur.i} */

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fi_BeginDate fi_EndDate btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_BeginDate fi_EndDate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fi_BeginDate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_EndDate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2099 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 3.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_BeginDate AT ROW 6.57 COL 24.4 COLON-ALIGNED HELP
          "Enter Beginning Date"
     fi_EndDate AT ROW 6.57 COL 59.4 COLON-ALIGNED HELP
          "Enter Ending Date"
     btn-process AT ROW 9.1 COL 24
     btn-cancel AT ROW 9.1 COL 56
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.67.

DEFINE FRAME FRAME-B
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Purge MU2 Receipts by Date"
         HEIGHT             = 10.33
         WIDTH              = 91.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 101.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 101.2
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge MU2 Receipts by Date */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge MU2 Receipts by Date */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:07:07 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR lProcess AS LOG INIT NO NO-UNDO.


  MESSAGE "Are you sure you want to purge receipts in MU2 " +
          "for the selected date range?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lProcess.

  IF lProcess THEN RUN RunProcess.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:07:07 pm */
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
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:07:07 pm */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*   /* check security */                */
/*   IF access-close THEN DO:            */
/*      APPLY "close" TO THIS-PROCEDURE. */
/*      RETURN .                         */
/*   END.                                */

  RUN enable_UI.
  {methods/nowait.i}
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:18 pm */
    {methods/setButton.i btn-process "Start"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:18 pm */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:07:07 pm */
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
  DISPLAY fi_BeginDate fi_EndDate 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fi_BeginDate fi_EndDate btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunProcess C-Win 
PROCEDURE RunProcess :
/* ------------------------------------------------ util/PurgeMU2.r 03/13 BPV */
/* Purge MU2 records by date range                                            */
/* -------------------------------------------------------------------------- */

DEFINE VARIABLE liCount         AS INT          NO-UNDO.
DEFINE VARIABLE llNKRmTags      AS LOG          NO-UNDO.
DEFINE VARIABLE llRecFound    AS LOG          NO-UNDO.
DEFINE VARIABLE lcRtnChar     AS CHARACTER    NO-UNDO.
DEFINE BUFFER   bf-rm-rctd      FOR rm-rctd.
DEFINE BUFFER   bf-wiptag       FOR wiptag.

SESSION:SET-WAIT-STATE("General").
DO WITH FRAME {&frame-name}:
    ASSIGN
        fi_BeginDate
        fi_EndDate.
END.

/* DISABLE TRIGGERS FOR LOAD OF ap-inv. */
/* DISABLE TRIGGERS FOR LOAD OF ap-pay. */

/*Get RMTAGS NK1 system setting*/
llNKRmTags = NO.
RUN sys/ref/nk1look.p (cocode, 
                       "RMTAGS", 
                       "L", 
                       NO, 
                       NO, 
                       "", 
                       "",
                       OUTPUT lcRtnChar,
                       OUTPUT llRecFound).
IF llRecFound THEN llNKRmTags = LOGICAL(lcRtnChar).

liCount = 0.
FOR EACH bf-rm-rctd 
    WHERE bf-rm-rctd.company EQ cocode 
        AND bf-rm-rctd.rita-code EQ "I"
        AND bf-rm-rctd.rct-date GE fi_BeginDate
        AND bf-rm-rctd.rct-date LE fi_EndDate
    EXCLUSIVE-LOCK:
    liCount = liCount + 1.
    /*Also delete corresponding wiptag records if NKRmTags*/
    IF llNKRmTags THEN 
        FOR EACH bf-wiptag WHERE bf-wiptag.company = bf-rm-rctd.company 
               AND bf-wiptag.sts = "Issued" 
               AND bf-wiptag.rm-tag-no = bf-rm-rctd.tag 
        EXCLUSIVE-LOCK:

            DELETE bf-wiptag .
        END.
        DELETE bf-rm-rctd. 
END. /*each bf-rm-rctd*/

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " process is completed. " + STRING(liCount) " records purged."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

APPLY "close" TO THIS-PROCEDURE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

