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

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF TEMP-TABLE tt-rm-bin NO-UNDO LIKE rm-bin
                                 FIELD trans-date LIKE rm-rcpth.trans-date.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 begin_rm-no end_rm-no begin_date end_date ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 begin_rm-no end_rm-no begin_date end_date 

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
     LABEL "From Issue Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Issue Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 5.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_job-no AT ROW 6.48 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 6.48 COL 36 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 6.48 COL 62 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 6.48 COL 75 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_rm-no AT ROW 7.67 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 7.67 COL 62 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_date AT ROW 8.86 COL 23 COLON-ALIGNED HELP
          "From Receipt Date"
     end_date AT ROW 8.86 COL 62 COLON-ALIGNED HELP
          "Enter Ending Receipt Date"
     btn-process AT ROW 11 COL 23
     btn-cancel AT ROW 11 COL 52
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 11.81.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 79 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 3
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
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Fix RM Issue Cost"
         HEIGHT             = 11.86
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Fix RM Issue Cost */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix RM Issue Cost */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* From Issue Date */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning RM Item# */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* To Issue Date */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending RM Item# */
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  APPLY "entry" TO begin_job-no.

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
  DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 begin_rm-no 
          end_rm-no begin_date end_date 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_job-no begin_job-no2 end_job-no end_job-no2 begin_rm-no 
         end_rm-no begin_date end_date btn-process btn-cancel 
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
DEF BUFFER b-rh FOR rm-rcpth.
DEF BUFFER b-rd FOR rm-rdtlh.

DEF VAR v-job-no LIKE job.job-no EXTENT 2 INIT ["", "zzzzzz"] NO-UNDO.
DEF VAR v-job-no2 LIKE job.job-no2 EXTENT 2 INIT [00, 99] NO-UNDO.

DEF VAR v-r-qty     AS   DEC                    NO-UNDO.
DEF VAR v-i-qty     AS   DEC                    NO-UNDO.
DEF VAR v-t-qty     AS   DEC                    NO-UNDO.
DEF VAR cost        AS   DEC                    NO-UNDO.
DEF VAR out-qty     AS   DEC                    NO-UNDO.
DEF VAR v-bwt       LIKE item.basis-w           NO-UNDO.
DEF VAR v-len       LIKE item.s-len             NO-UNDO.
DEF VAR v-wid       LIKE item.s-wid             NO-UNDO.
DEF VAR v-dep       LIKE item.s-dep             NO-UNDO.
DEF VAR v-recid     AS   RECID                  NO-UNDO.
DEF VAR li          AS   INT                    NO-UNDO.
DEF VAR lv-uom      AS   CHAR                   NO-UNDO.
DEF VAR ld-qty      AS   DEC                    NO-UNDO.
DEF VAR ld-cst      AS   DEC                    NO-UNDO.


SESSION:SET-WAIT-STATE("general").

ASSIGN
 v-job-no[1] = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
               TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
 v-job-no[2] = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
               TRIM(end_job-no)   + STRING(INT(end_job-no2),"99").

FOR EACH b-rh NO-LOCK
    WHERE b-rh.company    EQ cocode
      AND b-rh.job-no     GE SUBSTR(v-job-no[1],1,6)
      AND b-rh.job-no     LE SUBSTR(v-job-no[2],1,6)
      AND FILL(" ",6 - LENGTH(TRIM(b-rh.job-no))) +
          TRIM(b-rh.job-no) + STRING(INT(b-rh.job-no2),"99") GE v-job-no[1]
      AND FILL(" ",6 - LENGTH(TRIM(b-rh.job-no)))   +
          TRIM(b-rh.job-no) + STRING(INT(b-rh.job-no2),"99") LE v-job-no[2]
      AND b-rh.i-no       GE begin_rm-no
      AND b-rh.i-no       LE end_rm-no
      AND b-rh.trans-date GE begin_date
      AND b-rh.trans-date LE end_date
      AND b-rh.rita-code  EQ "I"
    USE-INDEX job,

    FIRST job NO-LOCK
    WHERE job.company EQ b-rh.company
      AND job.job-no  EQ b-rh.job-no
      AND job.job-no2 EQ b-rh.job-no2,

    FIRST item NO-LOCK
    WHERE item.company  EQ b-rh.company
      AND item.i-no     EQ b-rh.i-no
      AND item.mat-type EQ "B",

    EACH b-rd
    WHERE b-rd.r-no      EQ b-rh.r-no
      AND b-rd.rita-code EQ b-rh.rita-code:

  STATUS DEFAULT "Processing Job/RMItem#: " +
                 TRIM(b-rh.job-no)          + "-" +
                 STRING(b-rh.job-no2,"99")  + "/" +
                 TRIM(b-rh.i-no).

  EMPTY TEMP-TABLE tt-rm-bin.

  FIND FIRST tt-rm-bin
      WHERE tt-rm-bin.company EQ b-rh.company
        AND tt-rm-bin.i-no    EQ b-rh.i-no
        AND tt-rm-bin.loc     EQ b-rd.loc
        AND tt-rm-bin.loc-bin EQ b-rd.loc-bin
        AND tt-rm-bin.tag     EQ b-rd.tag
      USE-INDEX loc-bin NO-ERROR.
  IF NOT AVAIL tt-rm-bin THEN DO:
    CREATE tt-rm-bin.
    ASSIGN
     tt-rm-bin.company = b-rh.company
     tt-rm-bin.loc     = b-rd.loc
     tt-rm-bin.loc-bin = b-rd.loc-bin
     tt-rm-bin.tag     = b-rd.tag
     tt-rm-bin.i-no    = b-rh.i-no.
  END.

  IF tt-rm-bin.tag NE "" THEN
  FOR EACH rm-rdtlh NO-LOCK
      WHERE rm-rdtlh.company EQ tt-rm-bin.company
        AND rm-rdtlh.loc     EQ tt-rm-bin.loc
        AND rm-rdtlh.loc-bin EQ tt-rm-bin.loc-bin
        AND rm-rdtlh.tag     EQ tt-rm-bin.tag
      USE-INDEX tag,

      FIRST rm-rcpth NO-LOCK
      WHERE rm-rcpth.r-no         EQ rm-rdtlh.r-no
        AND rm-rcpth.rita-code    EQ rm-rdtlh.rita-code
        AND rm-rcpth.i-no         EQ tt-rm-bin.i-no
        AND (rm-rcpth.trans-date  LT b-rh.trans-date OR
             (rm-rcpth.trans-date EQ b-rh.trans-date AND
              rm-rcpth.r-no       LT b-rh.r-no))
      USE-INDEX r-no

      BY rm-rcpth.trans-date
      BY rm-rcpth.r-no
      BY RECID(rm-rcpth)
      BY RECID(rm-rdtlh):

    {rm/rm-mkbin.i tt-}
  END. /* each rm-rcpth */

  ELSE
  FOR EACH rm-rcpth NO-LOCK
      WHERE rm-rcpth.company      EQ tt-rm-bin.company
        AND rm-rcpth.i-no         EQ tt-rm-bin.i-no
        AND (rm-rcpth.trans-date  LT b-rh.trans-date OR
             (rm-rcpth.trans-date EQ b-rh.trans-date AND
              rm-rcpth.r-no       LT b-rh.r-no))
      USE-INDEX i-no,

      EACH rm-rdtlh NO-LOCK
      WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
        AND rm-rdtlh.loc       EQ tt-rm-bin.loc
        AND rm-rdtlh.loc-bin   EQ tt-rm-bin.loc-bin
        AND rm-rdtlh.tag       EQ tt-rm-bin.tag
      USE-INDEX rm-rdtl

      BY rm-rcpth.trans-date
      BY rm-rcpth.r-no
      BY RECID(rm-rcpth)
      BY RECID(rm-rdtlh):

    {rm/rm-mkbin.i tt-}
  END. /* each rm-rcpth */

  RELEASE job-mat.
  FOR EACH job-mat
      WHERE job-mat.company  EQ job.company
        AND job-mat.job      EQ job.job
        AND job-mat.job-no   EQ job.job-no
        AND job-mat.job-no2  EQ job.job-no2
        AND job-mat.frm      EQ b-rd.s-num
        AND job-mat.i-no     EQ b-rh.i-no
      NO-LOCK
      BREAK BY job-mat.blank-no DESC:

    IF LAST(job-mat.blank-no) OR job-mat.blank-no EQ b-rd.b-num THEN LEAVE.
  END.

  IF AVAIL job-mat THEN DO:
    ASSIGN
     v-bwt = job-mat.basis-w
     v-len = job-mat.len
     v-wid = job-mat.wid
     v-dep = item.s-dep.

    IF v-len EQ 0 THEN v-len = item.s-len.

    IF v-wid EQ 0 THEN
      v-wid = IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid.

    IF v-bwt EQ 0 THEN v-bwt = item.basis-w.

    IF item.cons-uom EQ b-rh.pur-uom THEN
      b-rd.cost = tt-rm-bin.cost.
    ELSE
      RUN sys/ref/convcuom.p(item.cons-uom, b-rh.pur-uom,
                             v-bwt, v-len, v-wid, v-dep,
                             tt-rm-bin.cost, OUTPUT b-rd.cost).    

    FIND FIRST mat-act
        WHERE mat-act.company   EQ job-mat.company
          AND mat-act.mat-date  EQ b-rh.post-date
          AND mat-act.job       EQ job.job
          AND mat-act.job-no    EQ job-mat.job-no
          AND mat-act.job-no2   EQ job-mat.job-no2
          AND mat-act.s-num     EQ job-mat.frm
          AND mat-act.b-num     EQ job-mat.blank-no
          AND mat-act.i-no      EQ job-mat.i-no
          AND mat-act.rm-i-no   EQ job-mat.i-no
          AND mat-act.tag       EQ b-rd.tag
          AND mat-act.loc       EQ b-rd.loc
          AND mat-act.loc-bin   EQ b-rd.loc-bin
        NO-ERROR.

    IF AVAIL mat-act THEN DO:
      IF b-rh.pur-uom EQ job-mat.sc-uom THEN
        mat-act.cost = b-rd.cost.
      ELSE
        RUN sys/ref/convcuom.p(b-rh.pur-uom, job-mat.sc-uom,
                               v-bwt, v-len, v-wid, v-dep,
                               b-rd.cost, OUTPUT mat-act.cost).

      mat-act.ext-cost = mat-act.cost * mat-act.qty.
    END.
  END.
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

