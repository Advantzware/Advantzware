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
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_est end_est fi_file_path ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_est end_est fi_file_path 

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

DEFINE VARIABLE fi_file_path AS CHARACTER FORMAT "X(75)" 
     LABEL "Estimates Files Path" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 5.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_est AT ROW 7.57 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Estimate Number"
     end_est AT ROW 7.57 COL 67 COLON-ALIGNED HELP
          "Enter Ending Estimate Number"
     fi_file_path AT ROW 8.76 COL 26 COLON-ALIGNED HELP
          "Enter File Path" WIDGET-ID 6
     btn-process AT ROW 11.71 COL 21
     btn-cancel AT ROW 11.71 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 6.38 COL 5
     "Load Estimates Archived from NF2" VIEW-AS TEXT
          SIZE 33 BY .62 AT ROW 5.1 COL 30 WIDGET-ID 8
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 13.52.

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
         TITLE              = "Load Estimates"
         HEIGHT             = 13.52
         WIDTH              = 90.2
         MAX-HEIGHT         = 13.52
         MAX-WIDTH          = 90.2
         VIRTUAL-HEIGHT     = 13.52
         VIRTUAL-WIDTH      = 90.2
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

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Load Estimates */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Load Estimates */
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
ON HELP OF fi_file_path IN FRAME FRAME-A /* Estimates Files Path */
DO:
  DEF VAR v-file-path AS CHAR NO-UNDO.

  SYSTEM-DIALOG GET-DIR v-file-path
     TITLE "Select Estimates Files Path".

  fi_file_path:SCREEN-VALUE = v-file-path + "\".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file_path C-Win
ON LEAVE OF fi_file_path IN FRAME FRAME-A /* Estimates Files Path */
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
  DISPLAY begin_est end_est fi_file_path 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_est end_est fi_file_path btn-process btn-cancel 
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

DEF VAR v-file-path AS CHAR FORMAT "X(75)" NO-UNDO.
DEF VAR v-est-no AS INT NO-UNDO.
DEF VAR v-est-char AS CHAR no-undo.
DEF VAR tmp-dir AS CHAR NO-UNDO.
DEF VAR viDirCount AS INT NO-UNDO.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.

do with frame {&frame-name}:
  assign
     begin_est
     end_est
     fi_file_path
     v-file-path = fi_file_path
     v-process = no.
end.

IF TRIM(v-file-path) EQ "" THEN
DO:
   MESSAGE "Estimate Files Path Cannot Be Blank."
       VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   APPLY "ENTRY" TO fi_file_path.
   LEAVE.
END.

IF NOT(SUBSTRING(v-file-path,LENGTH(v-file-path),1) = "/" OR
   SUBSTRING(v-file-path,LENGTH(v-file-path),1) = "\") THEN
   v-file-path = v-file-path + "/".

FILE-INFO:FILENAME = v-file-path.

IF index(FILE-INFO:FILE-TYPE,"D") EQ 0 THEN
DO:
   MESSAGE "Invalid Estimates Files Path."
       VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   APPLY "ENTRY" TO fi_file_path.
   LEAVE.
END.

message "Are you sure you want to load the estimates within the " +
        "selection parameters?"
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

  DISABLE TRIGGERS FOR LOAD OF e-item-vend.
  DISABLE TRIGGERS FOR LOAD OF e-itemfg-vend.
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
  DISABLE TRIGGERS FOR LOAD OF notes.
  DISABLE TRIGGERS FOR LOAD OF probe.
  DISABLE TRIGGERS FOR LOAD OF probeit.
  DISABLE TRIGGERS FOR LOAD OF notes.
  DISABLE TRIGGERS FOR LOAD OF box-design-line.
  DISABLE TRIGGERS FOR LOAD OF box-design-hdr.
  DISABLE TRIGGERS FOR LOAD OF est.

  DO v-est-no = begin_est TO end_est:

     v-est-char = string(v-est-no,">>>>>>>>").

     FILE-INFO:FILE-NAME = v-file-path + "e-item-vend" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE e-item-vend.
          IMPORT e-item-vend.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "e-itemfg-vend" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE e-itemfg-vend.
          IMPORT e-itemfg-vend.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "eb" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE eb.
          IMPORT eb.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "reftable" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE reftable.
          IMPORT reftable.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "ef" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE ef.
          IMPORT ef.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "ef-nsh" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE ef-nsh.
          IMPORT ef-nsh.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "est-flm" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE est-flm.
          IMPORT est-flm.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "est-inst" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE est-inst.
          IMPORT est-inst.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "est-op" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE est-op.
          IMPORT est-op.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "est-prep" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE est-prep.
          IMPORT est-prep.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "est-qty" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE est-qty.
          IMPORT est-qty.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "est-summ" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE est-summ.
          IMPORT est-summ.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "notes" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE notes.
          IMPORT notes.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "probe" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE probe.
          IMPORT probe.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "probeit" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE probeit.
          IMPORT probeit.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "box-design-line" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE box-design-line.
          IMPORT box-design-line.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "box-design-hdr" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE box-design-hdr.
          IMPORT box-design-hdr.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "est" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE est.
          IMPORT est.
        END.

        INPUT CLOSE.
     END.

     FILE-INFO:FILE-NAME = v-file-path + "notes" + v-est-char + ".d".

     IF FILE-INFO:FILE-SIZE <> ? THEN
     DO:
        INPUT FROM VALUE(FILE-INFO:FILE-NAME).

        REPEAT:
          CREATE notes.
          IMPORT notes.
        END.

        INPUT CLOSE.
     END.

     FOR EACH probe fields(LINE est-no) WHERE
         probe.company EQ cocode AND
         probe.est-no EQ v-est-char
         NO-LOCK:

         v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

         OS-COMMAND SILENT VALUE("copy " + v-file-path + TRIM(probe.est-no) + "-*.*" + STRING(probe.LINE,v-probe-fmt)
            + " " + tmp-dir).

         OS-COMMAND SILENT VALUE("copy " + v-file-path + TRIM(probe.est-no) +  ".*" + STRING(probe.LINE,v-probe-fmt)
            + " " + tmp-dir).
     END.

  END. /*end v-est-no*/

  session:set-wait-state("").

  message "Process Is Completed." view-as alert-box.
  apply "close" to this-procedure.
end.

return no-apply.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

