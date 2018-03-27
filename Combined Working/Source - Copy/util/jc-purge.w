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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

DEF TEMP-TABLE tt-po NO-UNDO FIELD po-no LIKE po-ordl.po-no
                             FIELD i-no AS CHAR
                             INDEX po-no po-no.
assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 tb_pending tb_released tb_on-hold tb_wip tb_closed ~
tb_mat-wip begin_rm-i-no end_rm-i-no tb_mch-wip begin_mach end_mach ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 tb_pending tb_released tb_on-hold tb_wip tb_closed tb_mat-wip ~
begin_rm-i-no end_rm-i-no tb_mch-wip begin_mach end_mach 

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

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Machine#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-i-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)":U 
     LABEL "Ending Machine#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-i-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Ending RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 13.57.

DEFINE VARIABLE tb_closed AS LOGICAL INITIAL yes 
     LABEL "Closed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_mat-wip AS LOGICAL INITIAL no 
     LABEL "Material WIP Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_mch-wip AS LOGICAL INITIAL no 
     LABEL "Machine WIP Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_on-hold AS LOGICAL INITIAL yes 
     LABEL "On Hold?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_pending AS LOGICAL INITIAL yes 
     LABEL "Pending?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_released AS LOGICAL INITIAL yes 
     LABEL "Released?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_wip AS LOGICAL INITIAL yes 
     LABEL "WIP?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_job-no AT ROW 6.24 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 6.24 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Job Number" NO-LABEL
     end_job-no AT ROW 6.24 COL 61 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 6.24 COL 76 COLON-ALIGNED HELP
          "Enter Ending Job Number" NO-LABEL
     tb_pending AT ROW 7.67 COL 45
     tb_released AT ROW 8.62 COL 45
     tb_on-hold AT ROW 9.57 COL 45
     tb_wip AT ROW 10.52 COL 45
     tb_closed AT ROW 11.48 COL 45
     tb_mat-wip AT ROW 12.91 COL 20
     begin_rm-i-no AT ROW 13.86 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_rm-i-no AT ROW 13.86 COL 67 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     tb_mch-wip AT ROW 15.76 COL 20
     begin_mach AT ROW 16.71 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_mach AT ROW 16.71 COL 67 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     btn-process AT ROW 19.33 COL 22
     btn-cancel AT ROW 19.33 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Job Status" VIEW-AS TEXT
          SIZE 11 BY 1 AT ROW 9.57 COL 32
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.2 BY 20.62.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
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
         TITLE              = "Purge Jobs"
         HEIGHT             = 20.71
         WIDTH              = 90.2
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
                                                                        */
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
ON END-ERROR OF C-Win /* Purge Jobs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Jobs */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR v-process AS LOG NO-UNDO.


  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
          " within the selection parameters?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  /* check security */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  RUN enable_UI.
  /* {custom/usrprint.i} */
  {methods/nowait.i}
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO begin_job-no.
  END.
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
  DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 tb_pending 
          tb_released tb_on-hold tb_wip tb_closed tb_mat-wip begin_rm-i-no 
          end_rm-i-no tb_mch-wip begin_mach end_mach 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_job-no begin_job-no2 end_job-no end_job-no2 tb_pending 
         tb_released tb_on-hold tb_wip tb_closed tb_mat-wip begin_rm-i-no 
         end_rm-i-no tb_mch-wip begin_mach end_mach btn-process btn-cancel 
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
/*************************************************** util/cln-job.p 03/97 FWK */
/* Purge Jobs Program                                                         */
/******************************************************************************/

def var v-job-no like job.job-no extent 2 initial [" ", " "] no-undo.
def var v-job-no2 like job.job-no2 extent 2 initial [00, 99] no-undo.
def var v-status like job.stat initial "*" no-undo.
def var stat-list as char initial "P,L,C,W,Z,*" no-undo.


session:set-wait-state("General").

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN {&displayed-objects}.
END.

EMPTY TEMP-TABLE tt-po.

assign
 v-job-no[1]  = begin_job-no
 v-job-no[2]  = end_job-no
 v-job-no2[1] = int(begin_job-no2)
 v-job-no2[2] = int(end_job-no2)
 v-status     = (if tb_pending  then "P"   else "") +
                (if tb_released then "ARL" else "") +
                (if tb_on-hold  then "H"   else "") +
                (if tb_wip      then "W"   else "") +
                (if tb_closed   then "CZ"  else "").

do x = 1 to 2:
  v-job-no[x] = fill(" ", 6 - integer(length(trim(v-job-no[x])))) +
                trim(v-job-no[x]).
end.

for each job
    where job.company eq cocode
      and job.job-no  ge v-job-no[1]
      and job.job-no  le v-job-no[2]
      and job.job-no2 ge v-job-no2[1]
      and job.job-no2 le v-job-no2[2]
      and index(v-status,job.stat) gt 0
    exclusive-lock

    transaction:

  IF tb_mat-wip EQ NO AND tb_mch-wip EQ NO THEN DO:
    run jc/jc-dall.p (recid(job)).

    for each job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job     eq job.job
          and job-hdr.job-no  eq job.job-no
          and job-hdr.job-no2 eq job.job-no2
        exclusive-lock:

      {util/dljobkey.i}          

      delete job-hdr.
    end.

    for each job-mat
        where job-mat.company eq job.company
          and job-mat.job     eq job.job
          and job-mat.job-no  eq job.job-no
          and job-mat.job-no2 eq job.job-no2
        exclusive-lock:
      delete job-mat.
    end.

    for each job-mch
        where job-mch.company eq job.company
          and job-mch.job     eq job.job
          and job-mch.job-no  eq job.job-no
          and job-mch.job-no2 eq job.job-no2
        exclusive-lock:
      delete job-mch.
    end.

    for each job-prep
        where job-prep.company eq job.company
          and job-prep.job     eq job.job
          and job-prep.job-no  eq job.job-no
          and job-prep.job-no2 eq job.job-no2
          exclusive-lock:
      delete job-prep.
    end.
  END.

  FOR EACH job-farm
      WHERE job-farm.company EQ job.company
        AND job-farm.job-no  EQ job.job-no
        AND job-farm.job-no2 EQ job.job-no2
      EXCLUSIVE:
    DELETE job-farm.
  END.

  FOR EACH job-farm-rctd
      WHERE job-farm-rctd.company EQ job.company
        AND job-farm-rctd.job-no  EQ job.job-no
        AND job-farm-rctd.job-no2 EQ job.job-no2
      EXCLUSIVE:
    DELETE job-farm-rctd.
  END.

  IF tb_mat-wip EQ YES OR tb_mch-wip EQ NO THEN
  for each mat-act
      where mat-act.company eq job.company
        and mat-act.job     eq job.job
        and mat-act.job-no  eq job.job-no
        and mat-act.job-no2 eq job.job-no2
        AND (tb_mat-wip EQ NO OR
             (mat-act.rm-i-no GE begin_rm-i-no AND
              mat-act.rm-i-no LE end_rm-i-no))
        use-index job exclusive-lock:
    delete mat-act.
  end.

  IF tb_mat-wip EQ NO OR tb_mch-wip EQ YES THEN
  for each mch-act
      where mch-act.company eq job.company
        and mch-act.job     eq job.job
        and mch-act.job-no  eq job.job-no
        and mch-act.job-no2 eq job.job-no2
        AND (tb_mch-wip EQ NO OR
             (mch-act.m-code GE begin_mach AND
              mch-act.m-code LE end_mach))
      use-index job exclusive-lock:
    delete mch-act.
  end.

  IF tb_mat-wip THEN
  DO:

  for each rm-rcpth WHERE
      rm-rcpth.company eq cocode AND
      rm-rcpth.job-no EQ job.job-no AND
      rm-rcpth.job-no2 EQ job.job-no2 AND
      rm-rcpth.i-no GE begin_rm-i-no AND
      rm-rcpth.i-no LE end_rm-i-no
      EXCLUSIVE-LOCK:

      IF TRIM(rm-rcpth.po-no) NE "" THEN DO:
         CREATE tt-po.
         ASSIGN tt-po.i-no = rm-rcpth.i-no
                tt-po.po-no = INT(rm-rcpth.po-no) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN tt-po.po-no = 0.
      END.

      for each rm-rdtlh WHERE
          rm-rdtlh.company eq cocode AND
          rm-rdtlh.r-no    eq rm-rcpth.r-no
          exclusive-lock:
          delete rm-rdtlh. 
      END.

      DELETE rm-rcpth.
  END. /* for each rm-rcpth */

  for each rm-rcpt WHERE
      rm-rcpt.company eq cocode AND
      rm-rcpt.job-no  eq job.job-no AND
      rm-rcpt.job-no2 EQ job.job-no2 AND
      rm-rcpt.i-no    GE begin_rm-i-no AND
      rm-rcpt.i-no    LE end_rm-i-no
      exclusive-lock:

      for each rm-rdtl WHERE
          rm-rdtl.company eq cocode AND
          rm-rdtl.r-no    eq rm-rcpt.r-no
          exclusive-lock:

          delete rm-rdtl.
      end. /* for each rm-rdtl */

      delete rm-rcpt. 
  end. /* for each rm-rcpt */

  FOR EACH tt-po WHERE tt-po.po-no GT 0 BREAK BY tt-po.po-no BY tt-po.i-no:
      IF LAST-OF(tt-po.i-no) THEN
        FOR EACH po-ord NO-LOCK
            WHERE po-ord.company EQ cocode
            AND po-ord.po-no   EQ tt-po.po-no,
            EACH po-ordl
            WHERE po-ordl.company EQ po-ord.company
            AND po-ordl.po-no   EQ po-ord.po-no
            AND po-ordl.i-no    EQ tt-po.i-no:

            RUN po/calc-rmr.p (BUFFER po-ordl).
        END.
  END. /*each tt-po*/
  END.

  IF tb_mat-wip EQ NO AND tb_mch-wip EQ NO THEN DO: 
    for each misc-act
        where misc-act.company eq job.company
          and misc-act.job     eq job.job
          and misc-act.job-no  eq job.job-no
          and misc-act.job-no2 eq job.job-no2
        exclusive-lock:
      delete misc-act.
    end.

    if job.exported then do:
      job.stat = "X".
      run jc/kiwiexp2.p (recid(job)).
    end.

    delete job.
  END.
end. /* each job */

session:set-wait-state("").

message trim(c-win:title) + " Process Is Completed." view-as alert-box.
apply "close" to this-procedure.

return no-apply.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

