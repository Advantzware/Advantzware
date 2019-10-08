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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-26 begin_est end_est ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_est end_est 

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

DEFINE VARIABLE begin_est AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_est AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 3.57.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 5.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_est AT ROW 8.14 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Estimate Number"
     end_est AT ROW 8.14 COL 66 COLON-ALIGNED HELP
          "Enter Ending Estimate Number"
     btn-process AT ROW 10.52 COL 21
     btn-cancel AT ROW 10.52 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 6.95 COL 5
     "This process is used to export estimates from this database so" VIEW-AS TEXT
          SIZE 72 BY 1 AT ROW 1.48 COL 9
          FONT 6
     "they may be imported to a DIFFERENT database with the Im-" VIEW-AS TEXT
          SIZE 72 BY 1 AT ROW 2.48 COL 9
          FONT 6
     "port Estimates process.  Please be sure to run the Import" VIEW-AS TEXT
          SIZE 72 BY 1 AT ROW 3.48 COL 9
          FONT 6
     "Estimates process on the other database and not this one." VIEW-AS TEXT
          SIZE 72 BY 1 AT ROW 4.48 COL 9
          FONT 6
     RECT-17 AT ROW 6.48 COL 1
     RECT-26 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 11.38.


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
         TITLE              = "Export Estimates"
         HEIGHT             = 11.52
         WIDTH              = 89.2
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
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Export Estimates */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Export Estimates */
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
  DEF VAR ll-process AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.

    MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) + " within the " +
            "selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-process.

    IF ll-process THEN RUN run-process.
  END.
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
  {methods/nowait.i}
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
  DISPLAY begin_est end_est 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 RECT-26 begin_est end_est btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------- util/exp-est.p 03/98 JLF */
/* Export Estimates                                                           */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def buffer boxd-hdr for box-design-hdr.
def buffer boxd-lin for box-design-line.

def var v-fest as int format ">>>>>" init 1.
def var v-test like v-fest init 99999.
def var v-fest-a like est.est-no.
def var v-test-a like v-fest-a.


session:set-wait-state("General").

assign
 v-fest   = begin_est
 v-test   = end_est
 v-fest-a = fill(" ",8 - length(trim(string(v-fest,">>>>>>>>")))) +
            trim(string(v-fest,">>>>>>>>"))
 v-test-a = fill(" ",8 - length(trim(string(v-test,">>>>>>>>")))) +
            trim(string(v-test,">>>>>>>>")).

{sys/inc/print1.i}

  output to value(tmp-dir + "~/" + "est-data.d").

  for each est
      where est.company eq cocode
        and est.est-no  ge v-fest-a
        and est.est-no  le v-test-a
      no-lock:

    put "est" skip.
    export est.

    for each eb of est where eb.form-no eq 0 no-lock:
      put "eb" skip.
      export eb except eb.rec_key eb.est-no.
    end.

    for each ef of est no-lock:
      put "ef" skip.
      export ef except ef.rec_key ef.est-no.

      for each eb of ef no-lock:
        put "eb" skip.
        export eb except eb.rec_key eb.est-no.

        for each boxd-hdr
            where boxd-hdr.design-no eq 0
              and boxd-hdr.company   eq eb.company
              and boxd-hdr.est-no    eq eb.est-no
              and boxd-hdr.form-no   eq eb.form-no
              and boxd-hdr.blank-no  eq eb.blank-no
            use-index design no-lock:

          put "boxd-hdr" skip.
          export boxd-hdr except boxd-hdr.rec_key boxd-hdr.est-no.

          for each boxd-lin of boxd-hdr:
            put "boxd-lin" skip.
            export boxd-lin except boxd-lin.rec_key boxd-lin.est-no.
          end.
        end.
      end.

      for each reftable
          where reftable.reftable eq "EST-MISC"
            and reftable.company  eq ef.company
            and reftable.loc      eq ef.loc
            and reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99")
          no-lock:
        put "reftable1" skip.
        export reftable except reftable.rec_key.
      end.
    end.

    FOR EACH est-prep
        WHERE est-prep.company EQ est.company
          AND est-prep.est-no  EQ est.est-no
        NO-LOCK:
      PUT "est-prep" skip.
      EXPORT est-prep EXCEPT est-prep.rec_key est-prep.est-no.
    END.

    FOR EACH est-op
        WHERE est-op.company EQ est.company
          AND est-op.est-no  EQ est.est-no
        NO-LOCK:
      PUT "est-op" SKIP.
      EXPORT est-op EXCEPT est-op.rec_key est-op.est-no.
    END.

    FOR EACH est-inst
        WHERE est-inst.company EQ est.company
          AND est-inst.est-no  EQ est.est-no
        NO-LOCK:
      PUT "est-inst" SKIP.
      EXPORT est-inst EXCEPT est-inst.rec_key est-inst.est-no.
    END.

    FOR EACH est-flm
        WHERE est-flm.company EQ est.company
          AND est-flm.est-no  EQ est.est-no
        NO-LOCK:
      PUT "est-flm" SKIP.
      EXPORT est-flm EXCEPT est-flm.rec_key est-flm.est-no.
    END.

    FOR EACH est-qty
        WHERE est-qty.company EQ est.company
          AND est-qty.est-no  EQ est.est-no
        NO-LOCK:
      PUT "est-qty" SKIP.
      EXPORT est-qty EXCEPT est-qty.rec_key est-qty.est-no.
    END.

    FOR EACH notes WHERE notes.rec_key EQ est.rec_key NO-LOCK:
      PUT "notes" SKIP.
      EXPORT notes EXCEPT notes.rec_key.
    END.

    FOR EACH reftable
        WHERE reftable.reftable EQ "est/getqty.w"
          AND reftable.company  EQ est.company
          AND reftable.loc      EQ est.loc
          AND reftable.code     EQ est.est-no
        NO-LOCK:
      PUT "reftable2" SKIP.
      EXPORT reftable EXCEPT reftable.rec_key reftable.code.
    END.

    FOR EACH probe
        WHERE probe.company EQ est.company
          AND probe.est-no  EQ est.est-no
        NO-LOCK:
      PUT "probe" SKIP.
      EXPORT probe EXCEPT probe.rec_key probe.est-no.
    END.
  END.

session:set-wait-state("").

message trim(c-win:title) + " Process Is Completed." view-as alert-box.

apply "close" to this-procedure.

/* end ---------------------------------- copr. 2004  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

