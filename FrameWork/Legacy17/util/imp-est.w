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

DEF TEMP-TABLE tt-est NO-UNDO LIKE est.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-26 fi_dir tb_est btn-cancel ~
btn-process 
&Scoped-Define DISPLAYED-OBJECTS fi_dir tb_est 

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

DEFINE VARIABLE fi_dir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Input from" 
     VIEW-AS FILL-IN 
     SIZE 74 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 6.19.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 5.48.

DEFINE VARIABLE tb_est AS LOGICAL INITIAL no 
     LABEL "Do NOT change estimate#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_dir AT ROW 8.14 COL 12 COLON-ALIGNED
     tb_est AT ROW 11 COL 31
     btn-cancel AT ROW 12.91 COL 53
     btn-process AT ROW 13.14 COL 21
     "Export Estimates process.  Please be sure you ran the Export" VIEW-AS TEXT
          SIZE 70 BY 1 AT ROW 3.48 COL 10
          FONT 6
     "Enter location of DB to be imported from" VIEW-AS TEXT
          SIZE 74 BY 1 AT ROW 9.33 COL 14
     "Estimates process on the other database and not this one." VIEW-AS TEXT
          SIZE 70 BY 1 AT ROW 4.48 COL 10
          FONT 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 6.95 COL 5
     "This process is used to import estimates to this database" VIEW-AS TEXT
          SIZE 70 BY 1 AT ROW 1.48 COL 10
          FONT 6
     "that were exported from a DIFFERENT database with the" VIEW-AS TEXT
          SIZE 70 BY 1 AT ROW 2.48 COL 10
          FONT 6
     RECT-17 AT ROW 6.48 COL 1
     RECT-26 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 13.71.


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
         TITLE              = "Import Estimates"
         HEIGHT             = 13.71
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
   FRAME-NAME                                                           */
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
ON END-ERROR OF C-Win /* Import Estimates */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import Estimates */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
  /* check security */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  fi_dir = "c:~\rcode~\".

  RUN enable_UI.

  {methods/nowait.i}

    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-process "Start"} /* added by script _nonAdm1Images1.p */
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
  DISPLAY fi_dir tb_est 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 RECT-26 fi_dir tb_est btn-cancel btn-process 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------- util/imp-est.p 03/98 JLF */
/* Import Estimates                                                           */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def buffer boxd-hdr for box-design-hdr.
def buffer boxd-lin for box-design-line.

def var v-file as char.
def var v-est-no like est.est-no.
def var v-rec_key like est.rec_key.
def var v-code like reftable.code.
def var v-in as char format "x(40)".
def var v-in-lab like v-in init "Enter location of DB to be imported from".
def var in-dir like tmp-dir.
DEF VAR ll-valid AS LOG NO-UNDO.


session:set-wait-state("General").

{sys/inc/print1.i}

DISABLE TRIGGERS FOR LOAD OF ef.

assign
 v-in   = fi_dir
 in-dir = tmp-dir.

  find first ce-ctrl {sys/look/ce-ctrlW.i} exclusive.

  if substr(v-in,length(trim(v-in)),1) ne "~\" then v-in = v-in + "~\".
  in-dir = trim(v-in).

  input from value(in-dir + "est-data.d").

  repeat:
    import v-file.

    IF v-file EQ "est" THEN DO:
      EMPTY TEMP-TABLE tt-est.

      CREATE tt-est.
      IMPORT tt-est.

      ll-valid = NOT tb_est OR
                 NOT CAN-FIND(FIRST est
                              WHERE est.company EQ tt-est.company
                                AND est.est-no  EQ tt-est.est-no).

      IF ll-valid THEN DO:
        CREATE est.
        BUFFER-COPY tt-est EXCEPT rec_key est-no TO est.

        IF tb_est THEN
          ASSIGN
           v-est-no  = tt-est.est-no
           v-rec_key = tt-est.rec_key.
        ELSE
          ASSIGN
           ce-ctrl.e-num = ce-ctrl.e-num + 1
           v-est-no      = STRING(ce-ctrl.e-num,">>>>>>>9").

        ASSIGN
         est.est-no = v-est-no
         v-rec_key  = est.rec_key.
      END.
    END.

    ELSE
    IF NOT ll-valid THEN NEXT.

    else
    if v-file eq "ef" then do:
      create ef.
      import ef except ef.rec_key ef.est-no.
      ef.est-no = v-est-no.
    end.

    else
    if v-file eq "eb"  then do:
      create eb.
      import eb except eb.rec_key eb.est-no.
      assign
       eb.est-no  = v-est-no
       eb.est-int = int(v-est-no).
    end.

    else
    if v-file eq "boxd-hdr"  then do:
      create boxd-hdr.
      import boxd-hdr except boxd-hdr.rec_key boxd-hdr.est-no.
      boxd-hdr.est-no = v-est-no.
    end.

    else
    if v-file eq "boxd-lin"  then do:
      create boxd-lin.
      import boxd-lin except boxd-lin.rec_key boxd-lin.est-no.
      boxd-lin.est-no = v-est-no.
    end.

    else
    if v-file eq "reftable1"  then do:
      create reftable.
      import reftable except reftable.rec_key.
      assign
       v-code        = reftable.code
       reftable.code = trim(v-est-no)
       k             = 0.
      do j = 1 to length(trim(v-code)):
            if substr(v-code,j,1) eq "/" then k = 1.
            if k eq 1 then
              reftable.code = trim(reftable.code) + substr(v-code,j,1).
      end.
    end.

    else
    if v-file eq "est-prep"  then do:
      create est-prep.
      import est-prep except est-prep.rec_key est-prep.est-no.
      est-prep.est-no = v-est-no.
    end.

    else
    if v-file eq "est-op"  then do:
      create est-op.
      import est-op except est-op.rec_key est-op.est-no.
      est-op.est-no = v-est-no.
    end.

    else
    if v-file eq "est-inst"  then do:
      create est-inst.
      import est-inst except est-inst.rec_key est-inst.est-no.
      est-inst.est-no = v-est-no.
    end.

    else
    if v-file eq "est-flm"  then do:
      create est-flm.
      import est-flm except est-flm.rec_key est-flm.est-no.
      est-flm.est-no = v-est-no.
    end.

    else
    if v-file eq "est-qty"  then do:
      create est-qty.
      import est-qty except est-qty.rec_key est-qty.est-no.
      est-qty.est-no = v-est-no.
    end.

    else
    if v-file eq "notes"  then do:
      create notes.
      import notes except notes.rec_key.
      notes.rec_key = v-rec_key.
    end.

    else
    if v-file eq "reftable2"  then do:
      create reftable.
      import reftable except reftable.rec_key reftable.code.
      reftable.code = v-est-no.
    end.

    else
    if v-file eq "probe"  then do:
      create probe.
      import probe except probe.rec_key probe.est-no.
      probe.est-no = v-est-no.
    end.
  end.

session:set-wait-state("").

message trim(c-win:title) + " Process Is Completed." view-as alert-box.

apply "close" to this-procedure.

/* end ---------------------------------- copr. 2004  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

