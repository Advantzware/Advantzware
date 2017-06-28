&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS beg_i-no end_i-no begin_job-no begin_job-no2 ~
end_job-no end_job-no2 TG_adj-pallet FI_units-per-pallet TG_adj-cost ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS beg_i-no end_i-no begin_job-no ~
begin_job-no2 end_job-no end_job-no2 TG_adj-pallet FI_units-per-pallet ~
TG_adj-cost 

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

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE beg_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "From FG Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "To FG Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FI_units-per-pallet AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Units per pallet" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE TG_adj-cost AS LOGICAL INITIAL no 
     LABEL "Reduce Pallet Cost" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG_adj-pallet AS LOGICAL INITIAL no 
     LABEL "Reduce Pallet Quantity" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     beg_i-no AT ROW 4.91 COL 16 COLON-ALIGNED
     end_i-no AT ROW 4.91 COL 56 COLON-ALIGNED
     begin_job-no AT ROW 6.1 COL 16 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 6.1 COL 31.2 COLON-ALIGNED HELP
          "Enter Beginning Job Number" NO-LABEL
     end_job-no AT ROW 6.1 COL 56 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 6.1 COL 71.2 COLON-ALIGNED HELP
          "Enter Ending Job Number" NO-LABEL
     TG_adj-pallet AT ROW 8 COL 40.6 WIDGET-ID 2
     FI_units-per-pallet AT ROW 8.29 COL 16 COLON-ALIGNED
     TG_adj-cost AT ROW 8.95 COL 40.6 WIDGET-ID 4
     btn-process AT ROW 10.38 COL 21
     btn-cancel AT ROW 10.38 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 14.52.

DEFINE FRAME FRAME-B
     "This utility will update the number of pallets." VIEW-AS TEXT
          SIZE 52 BY .95 AT ROW 2.19 COL 19.4
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.33
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
         TITLE              = "Update the number of pallets"
         HEIGHT             = 11
         WIDTH              = 90.2
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
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
ON END-ERROR OF C-Win /* Update the number of pallets */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update the number of pallets */
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
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME beg_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL beg_i-no C-Win
ON LEAVE OF beg_i-no IN FRAME FRAME-A /* From FG Item */
DO:
   ASSIGN {&self-name}.
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

   DEF VAR v-cnt AS INT NO-UNDO.
   DEF VAR v-qty AS DECI NO-UNDO.
   DEF VAR v-job-no LIKE job.job-no EXTENT 2 INITIAL [" ", " "] NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN {&DISPLAYED-OBJECTS}.
   END.

   SESSION:SET-WAIT-STATE("General"). 

   IF FI_units-per-pallet > 0 THEN DO:
      ASSIGN
         v-job-no[1]    = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
                          TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
         v-job-no[2]    = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
                          TRIM(end_job-no)   + STRING(INT(end_job-no2),"99").

      FOR EACH job-hdr NO-LOCK WHERE job-hdr.company = cocode
                                 AND job-hdr.i-no   GE beg_i-no
                                 AND job-hdr.i-no   LE end_i-no
                                 AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +                             
                                     TRIM(job-hdr.job-no) + STRING(INT(job-hdr.job-no2),"99") GE v-job-no[1] 
                                 AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +                             
                                     TRIM(job-hdr.job-no) + STRING(INT(job-hdr.job-no2),"99") LE v-job-no[2] :
         FOR EACH job-mat NO-LOCK WHERE job-mat.company = job-hdr.company
                                    AND job-mat.job     = job-hdr.job
                                    AND job-mat.job-no  = job-hdr.job-no
                                    AND job-mat.job-no2 = job-hdr.job-no2                                  
                                    AND job-mat.frm     = job-hdr.frm
                                    AND job-mat.blank-no = job-hdr.blank-no:
/*                                     AND job-mat.i-no    = job-hdr.i-no: */

            FIND FIRST ITEM WHERE item.company = job-mat.company
                              AND item.i-no    = job-mat.rm-i-no
                              AND item.mat-type = "D" NO-LOCK NO-ERROR.
            IF AVAILABLE(ITEM) THEN DO:
               FOR EACH mat-act WHERE mat-act.company  = job-hdr.company 
                                  AND mat-act.job-no   = job-hdr.job-no
                                  AND mat-act.job-no2  = job-hdr.job-no2
                                  AND mat-act.s-num    = job-hdr.frm 
                                  AND mat-act.b-num    = job-hdr.blank-no 
                                  AND mat-act.i-no     = ITEM.i-no:

                  IF TG_adj-pallet = YES THEN
                     mat-act.qty = mat-act.qty / FI_units-per-pallet.
                  IF TG_adj-cost = YES THEN
                     mat-act.ext-cost = mat-act.ext-cost / FI_units-per-pallet.

                  v-cnt = v-cnt + 1.
               END.
            END.
            STATUS DEFAULT "Processing.... " + TRIM(STRING(v-cnt)).
         END.
      END.

      MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
      STATUS DEFAULT "".
      SESSION:SET-WAIT-STATE("").
      APPLY "close" TO THIS-PROCEDURE.
   END.
   ELSE DO:
      MESSAGE "Units per pallet must be greater than 0."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      STATUS DEFAULT "".
      SESSION:SET-WAIT-STATE("").
   END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* To FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI_units-per-pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI_units-per-pallet C-Win
ON LEAVE OF FI_units-per-pallet IN FRAME FRAME-A /* Units per pallet */
DO:
   ASSIGN {&self-name}.
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
  DISPLAY beg_i-no end_i-no begin_job-no begin_job-no2 end_job-no end_job-no2 
          TG_adj-pallet FI_units-per-pallet TG_adj-cost 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE beg_i-no end_i-no begin_job-no begin_job-no2 end_job-no end_job-no2 
         TG_adj-pallet FI_units-per-pallet TG_adj-cost btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

