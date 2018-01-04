&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-prodmh.w

  Description: Production Highlights

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

{sys/inc/var.i new shared}
{salrep/dashprod.i NEW}

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD DATE AS DATE
    FIELD row-id AS ROWID
    FIELD qty AS DEC
    FIELD amt       LIKE ar-invl.amt        FORMAT "->>>>>>>9.99"
    FIELD cash-date LIKE ar-inv.inv-date
    FIELD misc AS LOG
    FIELD cost AS DEC
    FIELD msf AS DEC.

DEF TEMP-TABLE work-tmp NO-UNDO
   field job like job.job
   field frm like job-mch.frm
   field blank-no like job-mch.blank-no
   FIELD sort-field AS CHAR
   field dept as char format 'xx'
   field m-code like mach.m-code
   field pass like job-mch.pass
   field r-act-hrs as dec format '>>>>9.99'
   field m-act-hrs as dec format '>>>>9.99'
   field dt-chg-hrs as dec format '>>>>9.99'
   field dt-nochg-hrs as dec format '>>>>9.99'
   field qty as dec format '>>>>>>>>9'
   field msf as dec format '>>>>>.999'
   INDEX work-tmp job frm blank-no dept m-code pass sort-field.

def TEMP-TABLE w-data no-undo
  field w-sman-no   AS CHAR
  field w-sqft      LIKE itemfg.t-sqft format "->>>9.999"    extent 4
  field w-amt       like ar-inv.gross  format "->>>,>>9.99"  extent 4
  field w-cost      like ar-inv.t-cost format "->>,>>9.99"   extent 3
  FIELD w-msf       AS DEC EXTENT 3.

DEF BUFFER b-mach FOR mach.
DEFINE BUFFER bf-user-print FOR user-print .
DEF VAR counter AS INT NO-UNDO.
DEFINE VARIABLE iMach AS INTEGER INIT 15 NO-UNDO .
assign
 cocode = gcompany
 locode = gloc.

/* gdm - 03090905 */
DEF VAR v-runflg AS LOG INIT NO NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME browse-machine

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mach

/* Definitions for BROWSE browse-machine                                */
&Scoped-define FIELDS-IN-QUERY-browse-machine mach.m-code mach.m-dscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-machine   
&Scoped-define SELF-NAME browse-machine
&Scoped-define QUERY-STRING-browse-machine FOR EACH mach WHERE      mach.company = fi_company:SCREEN-VALUE AND      mach.loc = locode      NO-LOCK BY mach.m-code
&Scoped-define OPEN-QUERY-browse-machine OPEN QUERY {&SELF-NAME} FOR EACH mach WHERE      mach.company = fi_company:SCREEN-VALUE AND      mach.loc = locode      NO-LOCK BY mach.m-code.
&Scoped-define TABLES-IN-QUERY-browse-machine mach
&Scoped-define FIRST-TABLE-IN-QUERY-browse-machine mach


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-browse-machine}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 fi_as-of-date fi_company ~
browse-machine btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_as-of-date fi_company 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_as-of-date AS DATE FORMAT "99/99/9999":U 
     LABEL "As Of Date" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_company AS CHARACTER FORMAT "X(3)":U 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71.6 BY 14.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browse-machine FOR 
      mach SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browse-machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-machine C-Win _FREEFORM
  QUERY browse-machine NO-LOCK DISPLAY
      mach.m-code FORMAT "X(6)" width 10
mach.m-dscr FORMAT "X(20)" COLUMN-LABEL "Description"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 44.4 BY 7.19 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_as-of-date AT ROW 2.81 COL 12.2 COLON-ALIGNED
     fi_company AT ROW 2.81 COL 42.6 COLON-ALIGNED
     browse-machine AT ROW 5.62 COL 5.2
     btn-ok AT ROW 14.14 COL 15
     btn-cancel AT ROW 14.14 COL 30.4
     "Select Up To 15 Machines for Production:" VIEW-AS TEXT
          SIZE 48 BY .62 AT ROW 4.38 COL 2.8
          FONT 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     RECT-9 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72 BY 15.14.


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
         TITLE              = "Production Highlights"
         HEIGHT             = 15.14
         WIDTH              = 72
         MAX-HEIGHT         = 15.14
         MAX-WIDTH          = 72
         VIRTUAL-HEIGHT     = 15.14
         VIRTUAL-WIDTH      = 72
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* BROWSE-TAB browse-machine fi_company FRAME-A */
ASSIGN 
       fi_company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-machine
/* Query rebuild information for BROWSE browse-machine
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH mach WHERE
     mach.company = fi_company:SCREEN-VALUE AND
     mach.loc = locode
     NO-LOCK BY mach.m-code.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-machine */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Production Highlights */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Production Highlights */
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
   IF v-runflg THEN DO:

    SESSION:SET-WAIT-STATE("general").

    ASSIGN {&displayed-objects} fi_company fi_as-of-date.

    EMPTY TEMP-TABLE tt-raw-prod.
    EMPTY TEMP-TABLE work-tmp.
    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE w-data.

    IF NOT CAN-FIND(FIRST company WHERE
       company.company EQ fi_company) THEN
       DO:
          MESSAGE "Invalid Company."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY "ENTRY:U" TO fi_company IN FRAME {&FRAME-NAME}.
          LEAVE.
       END.

    IF BROWSE browse-machine:NUM-SELECTED-ROWS GT 15 THEN
    DO:
       MESSAGE "More than 15 Machines are Selected."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY:U" TO BROWSE browse-machine.
       LEAVE.
    END.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
     WHERE user-print.company    EQ cocode        
      AND user-print.program-id EQ "HM5" 
      AND user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.

    IF NOT AVAIL user-print THEN DO:
        CREATE user-print .
        ASSIGN
            user-print.company     = cocode            
            user-print.program-id  = "HM5"     
            user-print.user-id     = USERID(LDBNAME(1)) 
              .
    END.
    
    ASSIGN user-print.field-value = "" .

    do counter = 1 to BROWSE browse-machine:NUM-SELECTED-ROWS:
       BROWSE browse-machine:FETCH-SELECTED-ROW(counter).

       CREATE tt-raw-prod.
       ASSIGN tt-raw-prod.m-code = mach.m-code
              tt-raw-prod.DATE   = fi_as-of-date.
       RELEASE tt-raw-prod.
      
       ASSIGN user-print.field-value[counter] =  mach.m-code  .
    end.
    FIND CURRENT user-print NO-LOCK NO-ERROR .

    RUN run-report.

    SESSION:SET-WAIT-STATE("").
   END.
   ELSE
   IF NOT v-runflg THEN DO:

      MESSAGE 
          "Management Reports are available for purchase, please call ASI."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

      APPLY  "close" TO THIS-PROCEDURE.

   END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_company C-Win
ON ENTRY OF fi_company IN FRAME FRAME-A /* Company */
DO:
  SELF:MODIFIED = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_company C-Win
ON LEAVE OF fi_company IN FRAME FRAME-A /* Company */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      IF SELF:MODIFIED THEN
      DO:
         ASSIGN fi_company.

         CLOSE QUERY browse-machine.

         OPEN QUERY browse-machine FOR EACH mach WHERE
              mach.company = fi_company AND
              mach.loc = locode
              NO-LOCK BY mach.m-code.

         FOR EACH reftable WHERE
             reftable.reftable EQ "HM5" AND
             reftable.company EQ USERID("NOSWEAT") AND
             reftable.loc = fi_company:SCREEN-VALUE
             NO-LOCK,
             FIRST mach WHERE
                   mach.company = fi_company:SCREEN-VALUE AND
                   mach.loc = locode AND
                   mach.m-code = reftable.CODE
                   NO-LOCK:

             REPOSITION browse-machine TO ROWID ROWID(mach).
             IF browse-machine:SELECT-FOCUSED-ROW() THEN.
         END.

         FOR EACH b-mach WHERE
             b-mach.company EQ fi_company:SCREEN-VALUE AND
             b-mach.loc = locode
             NO-LOCK
             BY b-mach.m-code:

             LEAVE.
         END.

        IF AVAIL b-mach THEN
           REPOSITION browse-machine TO ROWID ROWID(b-mach).
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browse-machine
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


/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  ASSIGN
    fi_company = cocode.

  RUN enable_UI.

  {methods/nowait.i}
  {custom/usrprint.i}

  CLOSE QUERY browse-machine.

  OPEN QUERY browse-machine FOR EACH mach WHERE
       mach.company = fi_company:SCREEN-VALUE AND
       mach.loc = locode
       NO-LOCK BY mach.m-code.

  FIND FIRST bf-user-print NO-LOCK
     WHERE bf-user-print.company    EQ cocode        
      AND bf-user-print.program-id EQ "HM5" 
      AND bf-user-print.user-id EQ USERID(LDBNAME(1)) NO-ERROR.
  
  IF AVAIL bf-user-print THEN DO:
     DO i = 1 TO iMach:
         FIND FIRST mach NO-LOCK
          WHERE mach.company = fi_company:SCREEN-VALUE AND
                mach.loc = locode AND
                mach.m-code = bf-user-print.field-value[i] NO-ERROR.
          IF AVAIL mach AND bf-user-print.field-value[i] NE ""  THEN
              REPOSITION browse-machine TO ROWID ROWID(mach).
              IF browse-machine:SELECT-FOCUSED-ROW() THEN.
      END.
  END.    /*avail user-print */ 

  FOR EACH b-mach WHERE
      b-mach.company EQ fi_company:SCREEN-VALUE AND
      b-mach.loc = locode
      NO-LOCK
      BY b-mach.m-code:

      LEAVE.
  END.

  IF AVAIL b-mach THEN
     REPOSITION browse-machine TO ROWID ROWID(b-mach).

  DO WITH FRAME {&FRAME-NAME}:
     fi_as-of-date:SCREEN-VALUE = STRING(TODAY).
     APPLY "entry" TO fi_as-of-date.
  END.

  /* gdm - 03090905 */
  {salrep/SlsMgmt.i}

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
  DISPLAY fi_as-of-date fi_company 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-9 fi_as-of-date fi_company browse-machine btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE raw-prod-proc C-Win 
PROCEDURE raw-prod-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*DR1*/
   def var v-on as int no-undo.
   def var v-out as int no-undo.
   DEF VAR v-up AS INT NO-UNDO.
   DEF VAR std-hrs-var AS DEC NO-UNDO.
   DEF VAR tot-hrs-var AS DEC NO-UNDO.
   DEF VAR v-start-of-year AS DATE NO-UNDO.
   DEF VAR v-month-as-of-date AS INT NO-UNDO.

   EMPTY TEMP-TABLE tt-report.

   FIND FIRST period WHERE
        period.company EQ fi_company AND
        period.yr EQ YEAR(fi_as-of-date)
        NO-LOCK NO-ERROR.

   IF AVAIL period THEN
      v-start-of-year = period.pst.
   ELSE
      v-start-of-year = DATE(1,1,YEAR(fi_as-of-date)).

   v-month-as-of-date = MONTH(fi_as-of-date).

   FOR EACH tt-raw-prod,
       EACH mch-act FIELDS(company m-code op-date job frm blank-no dept pass
            CODE qty op-date hours) WHERE
            mch-act.company EQ fi_company AND
            mch-act.m-code  EQ tt-raw-prod.m-code AND
            mch-act.op-date GE v-start-of-year AND
            mch-act.op-date LE fi_as-of-date
            NO-LOCK:

       find first work-tmp where
            work-tmp.job = mch-act.job and
            work-tmp.frm = mch-act.frm and
            work-tmp.blank-no = mch-act.blank-no and
            work-tmp.dept = mch-act.dept and
            work-tmp.m-code = mch-act.m-code and
            work-tmp.pass = mch-act.pass
            no-error.

       find first b-mach WHERE
            b-mach.company eq fi_company AND
            b-mach.loc = locode AND
            b-mach.m-code  eq mch-act.m-code
            no-lock no-error.

       IF NOT AVAIL work-tmp THEN
       DO:
          CREATE work-tmp.
          ASSIGN
             work-tmp.job      = mch-act.job
             work-tmp.frm      = mch-act.frm
             work-tmp.blank-no = mch-act.blank-no
             work-tmp.dept     = IF mch-act.dept NE "" THEN mch-act.dept
                                 ELSE IF AVAIL b-mach THEN b-mach.dept[1] ELSE ""
             work-tmp.m-code   = mch-act.m-code
             work-tmp.pass     = mch-act.pass.
       END.

       FIND job-code WHERE
            job-code.code EQ mch-act.code
            NO-LOCK NO-ERROR.

       IF AVAIL job-code THEN
       DO:
           IF job-code.cat eq "RUN" THEN DO:

              work-tmp.qty = work-tmp.qty
                           + IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty.

              IF work-tmp.qty EQ ? THEN work-tmp.qty = 0.

              IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                 ASSIGN
                    tt-raw-prod.date-qty = tt-raw-prod.date-qty
                                         + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                    tt-raw-prod.date-run-hrs = tt-raw-prod.date-run-hrs
                                             + mch-act.hours.

              IF mch-act.op-date LE fi_as-of-date THEN
              DO:
                 IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                    ASSIGN
                       tt-raw-prod.mtd-qty = tt-raw-prod.mtd-qty
                                           + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                       tt-raw-prod.mtd-run-hrs = tt-raw-prod.mtd-run-hrs
                                               + mch-act.hours.

                 ASSIGN
                    tt-raw-prod.ytd-qty = tt-raw-prod.ytd-qty
                                        + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                    tt-raw-prod.ytd-run-hrs = tt-raw-prod.ytd-run-hrs
                                            + mch-act.hours.
              END.

              for each job-hdr FIELDS(company job frm i-no est-no frm blank-no n-on) WHERE
                  job-hdr.company   eq fi_company AND
                  job-hdr.job       eq work-tmp.job AND
                  job-hdr.frm       eq work-tmp.frm AND
                 (job-hdr.blank-no eq mch-act.blank-no or mch-act.blank-no eq 0)
                 no-lock,
                 first itemfg FIELDS(company i-no t-sqft) WHERE
                       itemfg.company eq fi_company AND
                       itemfg.i-no    eq job-hdr.i-no
                       no-lock:

                 assign
                    v-on  = 1
                    v-out = 1.

                 if avail b-mach and index("APB",b-mach.p-type) le 0 then do:
                    find first eb WHERE
                         eb.company   eq job-hdr.company AND
                         eb.est-no    EQ job-hdr.est-no AND
                         eb.form-no   eq job-hdr.frm AND
                         (eb.blank-no eq job-hdr.blank-no or job-hdr.blank-no eq 0)
                         no-lock no-error.

                     if avail eb then v-up = eb.num-up.

                     if job-hdr.n-on ne 0 then v-up = job-hdr.n-on.

                     find first ef
                         where ef.company eq job-hdr.company
                           and ef.est-no  EQ job-hdr.est-no
                           and ef.form-no eq job-hdr.frm
                         no-lock no-error.

                     IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).

                     v-on = v-up * v-out.

                     IF mch-act.blank-no NE 0 THEN
                     DO:
                        find first est-op WHERE
                             est-op.company eq job-hdr.company AND
                             est-op.est-no  EQ job-hdr.est-no AND
                             est-op.s-num   eq mch-act.frm AND
                             est-op.b-num  eq mch-act.blank-no AND
                             est-op.m-code  eq mch-act.m-code AND
                             est-op.op-pass eq mch-act.pass AND
                             est-op.dept    eq mch-act.dept AND
                             est-op.line    lt 500
                             no-lock no-error.

                        if not avail est-op then
                           find first est-op WHERE
                                est-op.company eq job-hdr.company AND   
                                est-op.est-no  EQ job-hdr.est-no AND   
                                est-op.s-num   eq mch-act.frm AND      
                                est-op.b-num  eq mch-act.blank-no AND   
                                est-op.op-pass eq mch-act.pass AND     
                                est-op.dept    eq mch-act.dept AND      
                                est-op.line    lt 500               
                                no-lock no-error.                         
                     END.
                     ELSE
                     DO:
                        find first est-op WHERE
                             est-op.company eq job-hdr.company AND
                             est-op.est-no  EQ job-hdr.est-no AND
                             est-op.s-num   eq mch-act.frm AND
                             est-op.m-code  eq mch-act.m-code AND
                             est-op.op-pass eq mch-act.pass AND
                             est-op.dept    eq mch-act.dept AND
                             est-op.line    lt 500
                             no-lock no-error.
                        if not avail est-op then
                           find first est-op WHERE
                                est-op.company eq job-hdr.company AND
                                est-op.est-no  EQ job-hdr.est-no AND
                                est-op.s-num   eq mch-act.frm AND
                                est-op.op-pass eq mch-act.pass AND
                                est-op.dept    eq mch-act.dept AND
                                est-op.line    lt 500
                                no-lock no-error.
                     END.

                     if avail est-op then
                       run sys/inc/numout.p (recid(est-op), output v-out).

                     else v-out = 1.

                     v-on = v-on / v-out.
                 end.

                 IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    tt-raw-prod.date-qty-msf = tt-raw-prod.date-qty-msf
                                             + (mch-act.qty * itemfg.t-sqft * v-on / 1000).

                 IF mch-act.op-date LE fi_as-of-date THEN
                 DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                       tt-raw-prod.mtd-qty-msf = tt-raw-prod.mtd-qty-msf
                                               + (mch-act.qty * itemfg.t-sqft * v-on / 1000).

                    tt-raw-prod.ytd-qty-msf = tt-raw-prod.ytd-qty-msf
                                            + (mch-act.qty * itemfg.t-sqft * v-on / 1000).
                 END.
              END.
           END.
           ELSE
              IF job-code.cat EQ "MR" THEN
              DO:
                 IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    tt-raw-prod.date-mr-hrs = tt-raw-prod.date-mr-hrs
                                            + mch-act.hours.

                 IF mch-act.op-date LE fi_as-of-date THEN
                 DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                       tt-raw-prod.mtd-mr-hrs = tt-raw-prod.mtd-mr-hrs
                                              + mch-act.hours.

                    tt-raw-prod.ytd-mr-hrs = tt-raw-prod.ytd-mr-hrs
                                           + mch-act.hours.
                 END.
              END.
           ELSE
              IF job-code.cat EQ "DT" THEN
              DO:
                 IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    tt-raw-prod.date-dt-charge = tt-raw-prod.date-dt-charge
                                               + mch-act.hours.

                 IF mch-act.op-date LE fi_as-of-date THEN
                 DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                       tt-raw-prod.mtd-dt-charge = tt-raw-prod.mtd-dt-charge
                                                 + mch-act.hours.


                    tt-raw-prod.ytd-dt-charge = tt-raw-prod.ytd-dt-charge
                                              + mch-act.hours.
                 END.
              END.
           ELSE
           DO:
              IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                 tt-raw-prod.date-dt-nc = tt-raw-prod.date-dt-nc
                                        + mch-act.hours.

              IF mch-act.op-date LE fi_as-of-date THEN
              DO:
                 IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                    tt-raw-prod.mtd-dt-nc = tt-raw-prod.mtd-dt-nc
                                          + mch-act.hours.

                 tt-raw-prod.ytd-dt-nc = tt-raw-prod.ytd-dt-nc
                                       + mch-act.hours.
              END.
           END.
       END. /*avail job-code */

   END. /*each tt-raw-prod*/

   FOR EACH work-tmp,
       FIRST tt-raw-prod WHERE
             tt-raw-prod.m-code EQ work-tmp.m-code:

       IF work-tmp.blank-no NE 0 THEN
       DO:
          find first job-mch where
               job-mch.company  = fi_company and
               job-mch.job      eq work-tmp.job and
               job-mch.frm      = work-tmp.frm and
               job-mch.blank-no = work-tmp.blank-no and
               job-mch.m-code   = work-tmp.m-code and
               job-mch.pass     = work-tmp.pass
               no-lock no-error.
          if not avail job-mch then
             find first job-mch where
                  job-mch.company eq fi_company and
                  job-mch.job      eq work-tmp.job and
                  job-mch.frm      eq work-tmp.frm and
                  job-mch.blank-no = work-tmp.blank-no and
                  job-mch.m-code   eq work-tmp.m-code
                  no-lock no-error.
       END.
       ELSE
       DO:
          find first job-mch where
               job-mch.company  = fi_company and
               job-mch.job      eq work-tmp.job and
               job-mch.frm      = work-tmp.frm and
               job-mch.m-code   = work-tmp.m-code and
               job-mch.pass     = work-tmp.pass
               no-lock no-error.
          if not avail job-mch then
             find first job-mch where
                  job-mch.company eq fi_company and
                  job-mch.job      eq work-tmp.job and
                  job-mch.frm      eq work-tmp.frm and
                  job-mch.m-code   eq work-tmp.m-code
                  no-lock no-error.
       END.

       if not avail job-mch then
       find first job-mch where job-mch.company eq fi_company and
                                job-mch.job     eq work-tmp.job and
                                job-mch.frm     eq work-tmp.frm and
                                job-mch.m-code  eq work-tmp.m-code and
                                job-mch.speed   ne 0
                                no-lock no-error.
       if not avail job-mch then
       find first job-mch where job-mch.company eq fi_company and
                                job-mch.job     eq work-tmp.job and
                                job-mch.frm     eq work-tmp.frm and
                                job-mch.m-code  eq work-tmp.m-code
                                no-lock no-error.

       if available job-mch then
       DO:
          std-hrs-var = (IF work-tmp.qty NE 0 AND job-mch.speed NE 0 THEN
                            work-tmp.qty / job-mch.speed ELSE job-mch.run-hr)
                      + job-mch.mr-hr.

          IF job-mch.start-date EQ tt-raw-prod.DATE THEN
             tt-raw-prod.date-std-hrs = tt-raw-prod.date-std-hrs
                                      + std-hrs-var.

          IF job-mch.start-date LE fi_as-of-date THEN
          DO:
             IF MONTH(job-mch.start-date) EQ v-month-as-of-date THEN
                tt-raw-prod.mtd-std-hrs = tt-raw-prod.mtd-std-hrs
                                        + std-hrs-var.

             tt-raw-prod.ytd-std-hrs = tt-raw-prod.ytd-std-hrs
                                     + std-hrs-var.
          END.
       end.

   END. /*each work-tmp*/

   FOR EACH tt-raw-prod:

       ASSIGN
       tt-raw-prod.date-eff = (IF (tt-raw-prod.date-run-hrs +
                               tt-raw-prod.date-mr-hrs +
                               tt-raw-prod.date-dt-charge) NE 0 THEN
                               (tt-raw-prod.date-std-hrs / (tt-raw-prod.date-run-hrs +
                               tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge) * 100)
                               ELSE 0)
       tt-raw-prod.date-util = (IF (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs +
                                tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) NE 0 THEN
                                (tt-raw-prod.date-std-hrs / (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) * 100)
                                ELSE 0)
       tt-raw-prod.date-dt-perc = (IF (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs +
                                tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) NE 0 THEN
                                (tt-raw-prod.date-dt-nc / (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) * 100)
                                ELSE 0)

       tt-raw-prod.mtd-eff = (IF (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs +
                              tt-raw-prod.mtd-dt-charge) NE 0 THEN
                              (tt-raw-prod.mtd-std-hrs / (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge) * 100)
                              ELSE 0)
       tt-raw-prod.mtd-util = (IF (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs +
                              tt-raw-prod.mtd-dt-charge + tt-raw-prod.mtd-dt-nc) NE 0 THEN
                              (tt-raw-prod.mtd-std-hrs / (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge +
                              tt-raw-prod.mtd-dt-nc) * 100)
                              ELSE 0)
       tt-raw-prod.mtd-dt-perc = (IF (tt-raw-prod.mtd-run-hrs +
                                 tt-raw-prod.mtd-mr-hrs +
                                 tt-raw-prod.mtd-dt-charge +
                                 tt-raw-prod.mtd-dt-nc) NE 0 THEN
                                 (tt-raw-prod.mtd-dt-nc / (tt-raw-prod.mtd-run-hrs +
                                 tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge +
                                 tt-raw-prod.mtd-dt-nc) * 100)
                                 ELSE 0)
       tt-raw-prod.ytd-eff = (IF (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs +
                              tt-raw-prod.ytd-dt-charge) NE 0 THEN
                              (tt-raw-prod.ytd-std-hrs / (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge) * 100)
                              ELSE 0)
       tt-raw-prod.ytd-util = (IF (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs +
                              tt-raw-prod.ytd-dt-charge +
                              tt-raw-prod.ytd-dt-nc) NE 0 THEN
                              (tt-raw-prod.ytd-std-hrs / (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge +
                              tt-raw-prod.ytd-dt-nc) * 100)
                              ELSE 0)
       tt-raw-prod.ytd-dt-perc = (IF (tt-raw-prod.ytd-run-hrs +
                                 tt-raw-prod.ytd-mr-hrs +
                                 tt-raw-prod.ytd-dt-charge +
                                 tt-raw-prod.ytd-dt-nc) NE 0 THEN
                                 (tt-raw-prod.ytd-dt-nc / (tt-raw-prod.ytd-run-hrs +
                                 tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge +
                                 tt-raw-prod.ytd-dt-nc) * 100)
                                 ELSE 0).
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DO WITH FRAME {&FRAME-NAME}:

   RUN raw-prod-proc. /*Raw Production*/
   RUN salrep\dashprod.p(INPUT fi_company,
                         INPUT fi_as-of-date).
   RUN custom\usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
END.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

