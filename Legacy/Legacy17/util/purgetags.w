&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_tag end_tag begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date tb_fg tb_cord ~
tb_rm tb_cjob tb_cl btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_tag end_tag begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date tb_fg tb_cord ~
tb_rm tb_cjob tb_cl 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_tag AS CHARACTER FORMAT "X(20)":U 
     LABEL "Beginning Tag" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_tag AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Tag" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.71.

DEFINE VARIABLE tb_cjob AS LOGICAL INITIAL no 
     LABEL "Closed Jobs Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cl AS LOGICAL INITIAL no 
     LABEL "Case Labels" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cord AS LOGICAL INITIAL no 
     LABEL "Closed Orders Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_fg AS LOGICAL INITIAL no 
     LABEL "Finished Goods" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_rm AS LOGICAL INITIAL no 
     LABEL "Raw Materials" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_tag AT ROW 6.95 COL 17 COLON-ALIGNED HELP
          "Enter Beginning Tag Number"
     end_tag AT ROW 6.95 COL 61 COLON-ALIGNED HELP
          "Enter Ending Tag Number"
     begin_job-no AT ROW 8.38 COL 17 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 8.38 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 8.38 COL 61 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 8.38 COL 73 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_date AT ROW 9.81 COL 17 COLON-ALIGNED HELP
          "Enter Beginning Purchase Order Number"
     end_date AT ROW 9.81 COL 61 COLON-ALIGNED HELP
          "Enter Ending Purchase Order Number"
     tb_fg AT ROW 11.48 COL 18
     tb_cord AT ROW 12.19 COL 53
     tb_rm AT ROW 12.67 COL 18
     tb_cjob AT ROW 13.38 COL 53
     tb_cl AT ROW 13.86 COL 18
     btn-process AT ROW 16.24 COL 26
     btn-cancel AT ROW 16.24 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.4 BY 17.33.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 11
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 6
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94 BY 3.81
         BGCOLOR 11 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge Tags"
         HEIGHT             = 17.48
         WIDTH              = 94.4
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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


ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Tags */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Tags */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
          " for the selected parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

  IF ll THEN RUN run-process.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
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

  RUN enable_UI.

  APPLY "entry" TO begin_tag.

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
  DISPLAY begin_tag end_tag begin_job-no begin_job-no2 end_job-no end_job-no2 
          begin_date end_date tb_fg tb_cord tb_rm tb_cjob tb_cl 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_tag end_tag begin_job-no begin_job-no2 end_job-no 
         end_job-no2 begin_date end_date tb_fg tb_cord tb_rm tb_cjob tb_cl 
         btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF VAR v-job-no LIKE job.job-no EXTENT 2 INIT [" ", "zzzzzz"] NO-UNDO.

DEF BUFFER b-loadtag FOR loadtag.


DISABLE TRIGGERS FOR LOAD OF loadtag.

SESSION:SET-WAIT-STATE("general").

ASSIGN
 v-job-no[1]   = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
                 TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
 v-job-no[2]   = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
                 TRIM(end_job-no)   + STRING(INT(end_job-no2),"99").

FOR EACH loadtag
    WHERE loadtag.company  EQ cocode
      AND loadtag.tag-no   GE begin_tag
      AND loadtag.tag-no   LE end_tag
      AND loadtag.tag-date GE begin_date
      AND loadtag.tag-date LE end_date
      and loadtag.job-no   GE SUBSTR(v-job-no[1],1,6)
      and loadtag.job-no   LE SUBSTR(v-job-no[2],1,6)
      AND FILL(" ",6 - LENGTH(TRIM(loadtag.job-no))) +
          TRIM(begin_job-no) + STRING(INT(loadtag.job-no2),"99") GE v-job-no[1]
      AND FILL(" ",6 - LENGTH(TRIM(loadtag.job-no))) +
          TRIM(begin_job-no) + STRING(INT(loadtag.job-no2),"99") LE v-job-no[2]:

  IF (tb_cl AND loadtag.is-case-tag EQ YES AND loadtag.item-type EQ NO)    OR
     (tb_fg AND loadtag.is-case-tag EQ NO  AND loadtag.item-type EQ NO)    OR
     (tb_rm AND loadtag.is-case-tag EQ NO  AND loadtag.item-type EQ YES)   THEN
    IF ((loadtag.item-type EQ NO AND
         NOT CAN-FIND(FIRST fg-bin
                      WHERE fg-bin.company EQ loadtag.company
                        AND fg-bin.tag     EQ loadtag.tag-no
                        AND fg-bin.qty     NE 0)) OR
        (loadtag.item-type EQ YES AND
         NOT CAN-FIND(FIRST rm-bin
                      WHERE rm-bin.company EQ loadtag.company
                        AND rm-bin.tag     EQ loadtag.tag-no
                        AND rm-bin.qty     NE 0)))  AND
       (NOT tb_cord OR
        CAN-FIND(FIRST oe-ord
                 WHERE oe-ord.company EQ loadtag.company
                   AND oe-ord.ord-no  EQ loadtag.ord-no
                   AND oe-ord.opened  EQ NO))       AND
       (NOT tb_cjob OR
        CAN-FIND(FIRST job
                 WHERE job.company EQ loadtag.company
                   AND job.job-no  EQ loadtag.job-no
                   AND job.job-no2 EQ loadtag.job-no2
                   AND job.opened  EQ NO))          AND
       CAN-FIND(FIRST b-loadtag
                WHERE b-loadtag.company EQ loadtag.company
                  AND b-loadtag.tag-no  BEGINS SUBSTR(loadtag.tag-no,1,15)
                  AND SUBSTR(b-loadtag.tag-no,1,15)
                                        EQ SUBSTR(loadtag.tag-no,1,15)
                  AND INT(SUBSTR(b-loadtag.tag-no,16,5))
                                        GT INT(SUBSTR(loadtag.tag-no,16,5)))
    THEN DO:

      STATUS DEFAULT "Processing Tag#: " + TRIM(loadtag.tag-no).

      DELETE loadtag.
    END.
END.

STATUS DEFAULT.

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Complete..." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

