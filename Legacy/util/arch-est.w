&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\arch-est.w

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

assign
 cocode = gcompany
 locode = gloc.

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
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_est end_est begin_cust ~
end_cust begin_est-date end_est-date begin_mod-date end_mod-date ~
begin_ord-date end_ord-date fi_file_path tb_archive tb_no-orders tb_folding ~
tb_corr btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_est end_est begin_cust end_cust ~
begin_est-date end_est-date begin_mod-date end_mod-date begin_ord-date ~
end_ord-date fi_file_path lbl_archive tb_archive lbl_no-order tb_no-orders ~
tb_folding tb_corr 

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_est AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_est-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Estimate Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mod-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Mod Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_est AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_est-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Estimate Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_mod-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Mod Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file_path AS CHARACTER FORMAT "X(75)" 
     LABEL "Archive Files Path" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1.

DEFINE VARIABLE lbl_archive AS CHARACTER FORMAT "X(256)":U INITIAL "Archive Also?" 
     VIEW-AS FILL-IN 
     SIZE 14.8 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_no-order AS CHARACTER FORMAT "X(256)":U INITIAL "Exclude Estimates with Orders Booked?" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 11.48.

DEFINE VARIABLE tb_archive AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_corr AS LOGICAL INITIAL yes 
     LABEL "Corrugated" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .95 NO-UNDO.

DEFINE VARIABLE tb_folding AS LOGICAL INITIAL yes 
     LABEL "Folding" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE tb_no-orders AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_est AT ROW 6.48 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Estimate Number"
     end_est AT ROW 6.48 COL 67 COLON-ALIGNED HELP
          "Enter Ending Estimate Number"
     begin_cust AT ROW 7.67 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 7.67 COL 67 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_est-date AT ROW 8.86 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Estiamte Date"
     end_est-date AT ROW 8.86 COL 67 COLON-ALIGNED HELP
          "Enter Ending Estimate Date"
     begin_mod-date AT ROW 10.05 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Modification Date"
     end_mod-date AT ROW 10.05 COL 67 COLON-ALIGNED HELP
          "Enter Ending Modification Date"
     begin_ord-date AT ROW 11.24 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Order Date"
     end_ord-date AT ROW 11.24 COL 67 COLON-ALIGNED HELP
          "Enter Ending Order Date"
     fi_file_path AT ROW 12.38 COL 47 COLON-ALIGNED HELP
          "Enter File Path" WIDGET-ID 6
     lbl_archive AT ROW 12.43 COL 9.4 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     tb_archive AT ROW 12.43 COL 26.4 WIDGET-ID 2
     lbl_no-order AT ROW 13.52 COL 9.2 COLON-ALIGNED NO-LABEL
     tb_no-orders AT ROW 13.52 COL 50.2
     tb_folding AT ROW 14.71 COL 35.4
     tb_corr AT ROW 14.71 COL 48.6
     btn-process AT ROW 17.24 COL 21
     btn-cancel AT ROW 17.24 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 18.52.

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
         TITLE              = "Archive/Delete Estimates"
         HEIGHT             = 17.71
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
       fi_file_path:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_archive IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_no-order IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Archive/Delete Estimates */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Archive/Delete Estimates */
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
   run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file_path
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file_path C-Win
ON HELP OF fi_file_path IN FRAME FRAME-A /* Archive Files Path */
DO:
  DEF VAR v-file-path AS CHAR NO-UNDO.

  SYSTEM-DIALOG GET-DIR v-file-path
     TITLE "Select Archive Files Path".

  fi_file_path:SCREEN-VALUE = v-file-path + "\".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file_path C-Win
ON LEAVE OF fi_file_path IN FRAME FRAME-A /* Archive Files Path */
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
  DISPLAY begin_est end_est begin_cust end_cust begin_est-date end_est-date 
          begin_mod-date end_mod-date begin_ord-date end_ord-date fi_file_path 
          lbl_archive tb_archive lbl_no-order tb_no-orders tb_folding tb_corr 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_est end_est begin_cust end_cust begin_est-date 
         end_est-date begin_mod-date end_mod-date begin_ord-date end_ord-date 
         fi_file_path tb_archive tb_no-orders tb_folding tb_corr btn-process 
         btn-cancel 
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
/* ------------------------------------------------ sys/del/est.p 3/29/95 CTS */
/* Delete / Archive old estimates                                             */
/* -------------------------------------------------------------------------- */

def var festno like est.est-no no-undo.
def var testno like festno no-undo.
def var fcust like eb.cust-no no-undo.
def var tcust like fcust init "zzzzzzzz" no-undo.
def var fest-date as date format "99/99/9999" init 01/01/0001 no-undo.
def var test-date like fest-date init "12/31/9999" no-undo.
def var fmod-date like fest-date no-undo.
def var tmod-date like test-date no-undo.
def var ford-date like fest-date no-undo.
def var tord-date like test-date no-undo.
def var archive as log format "Archive/Delete" no-undo.
DEF VAR v-file-path AS CHAR FORMAT "X(75)" NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR tmp-dir AS CHAR NO-UNDO.
DEF VAR viDirCount AS INT NO-UNDO.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.

do with frame {&frame-name}:
  assign
   begin_est
   end_est
   begin_cust
   end_cust
   begin_est-date
   end_est-date
   begin_mod-date
   end_mod-date
   begin_ord-date
   end_ord-date
   tb_no-orders
   tb_folding
   tb_corr
   tb_archive
   fi_file_path.
end.

assign
 festno    = string(begin_est,">>>>>>>>")
 testno    = string(end_est,">>>>>>>>")
 fcust     = begin_cust
 tcust     = end_cust
 fest-date = begin_est-date
 test-date = end_est-date
 fmod-date = begin_mod-date
 tmod-date = end_mod-date
 ford-date = begin_ord-date
 tord-date = end_ord-date
 archive   = tb_archive
 v-file-path = fi_file_path
 v-process = no.

 DISABLE TRIGGERS FOR LOAD OF eb.
 DISABLE TRIGGERS FOR LOAD OF reftable.
 DISABLE TRIGGERS FOR LOAD OF ef.
 DISABLE TRIGGERS FOR LOAD OF ef-nsh.
 DISABLE TRIGGERS FOR LOAD OF est-flm.
 DISABLE TRIGGERS FOR LOAD OF est-inst.
 DISABLE TRIGGERS FOR LOAD OF est-op.
 DISABLE TRIGGERS FOR LOAD OF est-prep.
 DISABLE TRIGGERS FOR LOAD OF est-qty.
 DISABLE TRIGGERS FOR LOAD OF est-summ.
 DISABLE TRIGGERS FOR LOAD OF probe.
 DISABLE TRIGGERS FOR LOAD OF probeit.
 DISABLE TRIGGERS FOR LOAD OF box-design-line.
 DISABLE TRIGGERS FOR LOAD OF box-design-hdr.
 DISABLE TRIGGERS FOR LOAD OF est.

IF archive THEN
DO:
   IF TRIM(v-file-path) EQ "" THEN
   DO:
      MESSAGE "Archive Files Path Cannot Be Blank."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "ENTRY" TO fi_file_path.
      LEAVE.
   END.

   IF NOT(SUBSTRING(v-file-path,LENGTH(v-file-path),1) = "/" OR
      SUBSTRING(v-file-path,LENGTH(v-file-path),1) = "\") THEN
      v-file-path = v-file-path + "/".

   FILE-INFO:FILE-NAME = v-file-path.

   IF index(FILE-INFO:FILE-TYPE,"D") EQ 0 THEN
   DO:
      MESSAGE "Invalid Estimates Files Path."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "ENTRY" TO fi_file_path.
      LEAVE.
   END.

   lv-msg = "Are you sure you want to archive/delete the estimates within the "
          + "selection parameters?".
END.
ELSE
   lv-msg = "Are you sure you want to delete the estimates within the "
          + "selection parameters?".

message lv-msg
        view-as alert-box question button yes-no update v-process.

if v-process then do:

  session:set-wait-state("General").

  find first sys-ctrl where
       sys-ctrl.company eq cocode AND
       sys-ctrl.name    eq "CEBROWSE"
       NO-LOCK.

  IF sys-ctrl.char-fld NE "" THEN
     tmp-dir = sys-ctrl.char-fld.
  ELSE
     tmp-dir = "users\".

  IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
     tmp-dir = tmp-dir + "\".

  tmp-dir = REPLACE(tmp-dir,"/","\").

  assign
   festno = fill(" ",8 - length(trim(festno))) + trim(festno)
   testno = fill(" ",8 - length(trim(testno))) + trim(testno).

  for each asi.est
      where asi.est.company    EQ cocode
        and asi.est.est-no     ge festno
        and asi.est.est-no     le testno
        and asi.est.est-date   ge fest-date
        and asi.est.est-date   le test-date
        and ((asi.est.mod-date ge fmod-date
        and   asi.est.mod-date le tmod-date) or
             asi.est.mod-date  eq ?)
        and ((asi.est.ord-date ge ford-date
        and   asi.est.ord-date le tord-date) or
             asi.est.ord-date  eq ?)
        AND ((asi.est.est-type LT 5 AND tb_folding) OR
             (asi.est.est-type GE 5 AND tb_corr))
        and can-find(first asi.eb where asi.eb.company eq asi.est.company
                                    and asi.eb.est-no  eq asi.est.est-no
                                    and asi.eb.cust-no ge fcust
                                    and asi.eb.cust-no le tcust
                                    and asi.eb.form-no ne 0)
      exclusive with frame f-est

      transaction:

    IF tb_no-orders AND asi.est.ord-no NE 0 THEN NEXT.

    IF archive THEN DO:
       output to value(v-file-path + "e-item-vend" + STRING(est.est-no,"X(8)") + ".d").
       OUTPUT CLOSE.
    END.

    for each asi.e-item-vend
        where asi.e-item-vend.company eq asi.est.company
          and asi.e-item-vend.est-no  eq asi.est.est-no
        exclusive with frame f-e-item-vend:

      if archive then do:
        output to value(v-file-path + "e-item-vend" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.e-item-vend.
        output close.
      end.

      delete asi.e-item-vend.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "e-itemfg-vend" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.e-itemfg-vend
        where asi.e-itemfg-vend.company eq asi.est.company
          and asi.e-itemfg-vend.est-no  eq asi.est.est-no
        exclusive with frame f-e-itemfg-vend:

      if archive then do:
        output to value(v-file-path + "e-itemfg-vend" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.e-itemfg-vend.
        output close.
      end.

      delete asi.e-itemfg-vend.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "eb" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.eb
        where asi.eb.company eq asi.est.company
          and asi.eb.est-no  eq asi.est.est-no
        exclusive with frame f-eb:

      if archive then do:
        output to value(v-file-path + "eb" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.eb.
        output close.
      end.

      delete asi.eb.
    end.

    /*Generate new reftable.d file*/
    IF archive THEN DO:
       output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d").
       OUTPUT CLOSE.
    END.

    IF archive THEN DO:
       output to value(v-file-path + "ef" + STRING(est.est-no,"X(8)") + ".d").
       OUTPUT close.
    END.

    for each asi.ef WHERE
        asi.ef.company eq asi.est.company AND
        asi.ef.est-no  eq asi.est.est-no
        exclusive with frame f-ef:

        for each reftable WHERE
            reftable.reftable eq "EST-MISC" AND
            reftable.company  eq ef.company AND
            reftable.loc      eq ef.loc AND
            reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99")
            exclusive with frame f-est-misc:

            if archive then do:
               output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
               export reftable.
               output close.
            end.

            delete reftable.
        END.

        if archive then do:
           output to value(v-file-path + "ef" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export asi.ef.
           output close.
        end.

        delete asi.ef.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "ef-nsh" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.ef-nsh
        where asi.ef-nsh.company eq asi.est.company
          and asi.ef-nsh.est-no  eq asi.est.est-no
        exclusive with frame f-ef-nsh:

      if archive then do:
        output to value(v-file-path + "ef-nsh" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.ef-nsh.
        output close.
      end.

      delete asi.ef-nsh.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "est-flm" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.est-flm
        where asi.est-flm.company eq asi.est.company
          and asi.est-flm.est-no  eq asi.est.est-no
        exclusive with frame f-est-flm:

      if archive then do:
        output to value(v-file-path + "est-flm" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.est-flm.
        output close.
      end.

      delete asi.est-flm.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "est-inst" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.est-inst
        where asi.est-inst.company eq asi.est.company
          and asi.est-inst.est-no  eq asi.est.est-no
        exclusive with frame f-est-inst:

      if archive then do:
        output to value(v-file-path + "est-inst" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.est-inst.
        output close.
      end.

      delete asi.est-inst.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "est-op" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.est-op
        where asi.est-op.company eq asi.est.company
          and asi.est-op.est-no  eq asi.est.est-no
        exclusive with frame f-est-op:

      if archive then do: 
        output to value(v-file-path + "est-op" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.est-op.
        output close.
      end.

      delete asi.est-op.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "est-prep" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.est-prep
        where asi.est-prep.company eq asi.est.company
          and asi.est-prep.est-no  eq asi.est.est-no
        exclusive with frame f-est-prep:

      if archive then do:
        output to value(v-file-path + "est-prep" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.est-prep.
        output close.
      end.

      delete asi.est-prep.
    end.

    IF archive THEN DO:
       OUTPUT to value(v-file-path + "est-qty" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.est-qty
        where asi.est-qty.company eq asi.est.company
          and asi.est-qty.est-no  eq asi.est.est-no
        exclusive with frame f-est-qty:

      if archive then do:
        output to value(v-file-path + "est-qty" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.est-qty.
        output close.
      end.

      delete asi.est-qty.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "est-summ" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.est-summ
        where asi.est-summ.company eq asi.est.company
          and asi.est-summ.est-no  eq asi.est.est-no
        exclusive with frame f-est-summ:

      if archive then do:
        output to value(v-file-path + "est-summ" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.est-summ.
        output close.
      end.

      delete asi.est-summ.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "notes" + STRING(est.est-no,"X(8)") + ".d").
       OUTPUT close.
    END.

    for each notes
        where notes.rec_key EQ est.rec_key
        exclusive with frame f-notes:

      if archive then do:
        output to value(v-file-path + "notes" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export notes.
        output close.
      end.

      delete notes.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "probe" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.probe
        where asi.probe.company eq asi.est.company
          and asi.probe.est-no  eq asi.est.est-no
        exclusive with frame f-probe:

      DO viDirCount = 1 TO 3:

         IF viDirCount EQ 2 THEN tmp-dir = "users\".
         ELSE IF viDirCount EQ 3 THEN tmp-dir = ".\".

         v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

         OS-COMMAND SILENT VALUE("copy " + tmp-dir + TRIM(probe.est-no) + "-*.*" + STRING(probe.LINE,v-probe-fmt)
            + " " + v-file-path).

         OS-COMMAND SILENT VALUE("copy " + tmp-dir + TRIM(probe.est-no) +  ".*" + STRING(probe.LINE,v-probe-fmt)
            + " " + v-file-path).

         DOS SILENT DEL VALUE(tmp-dir + TRIM(probe.est-no) + "-*.*" + STRING(probe.LINE,v-probe-fmt)).
         DOS SILENT DEL VALUE(tmp-dir + TRIM(probe.est-no) +   ".*" + STRING(probe.LINE,v-probe-fmt)).
      END.

      if archive then do:
        output to value(v-file-path + "probe" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.probe.
        output close.
      end.

      delete asi.probe.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "probeit" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.probeit
        where asi.probeit.company eq asi.est.company
          and asi.probeit.est-no  eq asi.est.est-no
        exclusive with frame f-probeit:

      if archive then do:
        output to value(v-file-path + "probeit" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.probeit.
        output close.
      end.

      delete asi.probeit.
    end.

    IF archive THEN DO:
       output to value(v-file-path + "box-design-line" + STRING(est.est-no,"X(8)") + ".d").
       output close.

       output to value(v-file-path + "box-design-hdr" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    for each asi.box-design-hdr
        where asi.box-design-hdr.company   eq asi.est.company
          and asi.box-design-hdr.est-no    eq asi.est.est-no
          and asi.box-design-hdr.design-no eq 0:

      for each asi.box-design-line of box-design-hdr exclusive:

        if archive then do:
          output to value(v-file-path + "box-design-line" + STRING(est.est-no,"X(8)") + ".d") APPEND.
          export asi.box-design-line.
          output close.
        end.

        delete asi.box-design-line.
      end.

      if archive then do:
        output to value(v-file-path + "box-design-hdr" + STRING(est.est-no,"X(8)") + ".d") APPEND.
        export asi.box-design-hdr.
        output close.
      end.

      delete asi.box-design-hdr.
    end.

       

    FOR EACH reftable WHERE
        reftable.reftable EQ "est/probeset.i" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ "" AND
        reftable.code     EQ est.est-no
        EXCLUSIVE WITH FRAME f-probeset:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    FOR EACH reftable WHERE
        reftable.reftable EQ "probe-ref" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ "" AND
        reftable.code     EQ est.est-no
        EXCLUSIVE WITH FRAME f-probe-ref:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    FOR EACH reftable WHERE
        reftable.reftable EQ "ce/com/probemk.p" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ est.est-no
        EXCLUSIVE WITH FRAME f-probemk:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    FOR EACH reftable WHERE
        reftable.reftable EQ "probe.per-msf" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ "" AND
        reftable.code     EQ est.est-no
        EXCLUSIVE WITH FRAME f-permsf:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    FOR EACH reftable WHERE
        reftable.reftable EQ "probe.per-ref" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ "" AND
        reftable.code     EQ est.est-no
        EXCLUSIVE WITH FRAME f-perref:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    FOR EACH reftable WHERE
        reftable.reftable EQ "probe.per-board" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ "" AND
        reftable.code     EQ est.est-no
        EXCLUSIVE WITH FRAME f-perbrd:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    
    
    FOR EACH reftable WHERE
        reftable.reftable EQ "est/getqty.w" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ est.loc AND
        reftable.code     EQ est.est-no
        EXCLUSIVE WITH FRAME f-getqty:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    FOR EACH reftable WHERE
        reftable.reftable EQ "est\d-multbl.w" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ est.loc AND
        reftable.code     EQ est.est-no
        EXCLUSIVE WITH FRAME f-dmultbl:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    FOR EACH reftable WHERE
        reftable.reftable EQ "est/d-grpcst.w" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ est.loc AND
        reftable.code     EQ est.est-no
        EXCLUSIVE WITH FRAME f-grpcst:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    FOR EACH reftable WHERE
        reftable.reftable EQ "PLATE/FOUNTAIN" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ est.est-no
        EXCLUSIVE WITH FRAME f-plate:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    FOR EACH reftable WHERE
        reftable.reftable EQ "cedepth" AND
        reftable.company  EQ est.company AND
        reftable.loc      EQ est.est-no
        EXCLUSIVE WITH FRAME f-depth:

        if archive then do:
           output to value(v-file-path + "reftable" + STRING(est.est-no,"X(8)") + ".d") APPEND.
           export reftable.
           output close.
        end.

        delete reftable.
    END.

    IF archive THEN DO:
       output to value(v-file-path + "est" + STRING(est.est-no,"X(8)") + ".d").
       output close.
    END.

    if archive then do:
      output to value(v-file-path + "est" + STRING(est.est-no,"X(8)") + ".d") APPEND.
      export asi.est.
      output close.
    end.

    delete asi.est.
  end.

  session:set-wait-state("").

  message "Process Is Completed." view-as alert-box.
  apply "close" to this-procedure.
end.

return no-apply.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

