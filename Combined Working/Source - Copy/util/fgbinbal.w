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
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_i-no end_i-no begin_cust ~
end_cust tb_del-zer tb_del-neg tbIncludeInactive tb_batch btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no begin_cust end_cust ~
lbl_del-zer tb_del-zer lbl_del-neg tb_del-neg lbl_Incl_inactive ~
tbIncludeInactive tb_batch 

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
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_del-neg AS CHARACTER FORMAT "X(256)":U INITIAL "Delete Bins With Negative Quantity?" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_del-zer AS CHARACTER FORMAT "X(256)":U INITIAL "Delete Bins With Zero Quantity?" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_Incl_inactive AS CHARACTER FORMAT "X(256)":U INITIAL "Include Inactive Items?" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 10.24.

DEFINE VARIABLE tbIncludeInactive AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .71 NO-UNDO.

DEFINE VARIABLE tb_batch AS LOGICAL INITIAL no 
     LABEL "Run In Batch Mode?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE tb_del-neg AS LOGICAL INITIAL no 
     LABEL "Delete Bins With Negative Quantity?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_del-zer AS LOGICAL INITIAL no 
     LABEL "Delete Bins With Zero Quantity?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_i-no AT ROW 6.95 COL 22.4 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_i-no AT ROW 6.95 COL 65.4 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cust AT ROW 8.14 COL 22.4 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 8.14 COL 65.4 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     lbl_del-zer AT ROW 10.29 COL 24 COLON-ALIGNED NO-LABEL
     tb_del-zer AT ROW 10.29 COL 58
     lbl_del-neg AT ROW 11.24 COL 20 COLON-ALIGNED NO-LABEL
     tb_del-neg AT ROW 11.29 COL 58
     lbl_Incl_inactive AT ROW 12.33 COL 31.4 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     tbIncludeInactive AT ROW 12.38 COL 58 WIDGET-ID 26
     tb_batch AT ROW 13.38 COL 58 WIDGET-ID 24
     btn-process AT ROW 16.48 COL 21
     btn-cancel AT ROW 16.48 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

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
         TITLE              = "Rebuild Bins/Reset Balances for FGs"
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


/* SETTINGS FOR FILL-IN lbl_del-neg IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_del-zer IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_Incl_inactive IN FRAME FRAME-A
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
ON END-ERROR OF C-Win /* Rebuild Bins/Reset Balances for FGs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Rebuild Bins/Reset Balances for FGs */
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  v-process = NO.

  MESSAGE "Are you sure you want to" TRIM(c-win:TITLE) +
          " within the selection parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE v-process.

  IF g_batch THEN tb_batch = YES.
  IF tb_batch THEN DO:
     RUN run-batch.
     RETURN NO-APPLY.
  END.
  ELSE IF v-process THEN RUN run-process.
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
  IF g_batch THEN tb_batch = YES.
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
  DISPLAY begin_i-no end_i-no begin_cust end_cust lbl_del-zer tb_del-zer 
          lbl_del-neg tb_del-neg lbl_Incl_inactive tbIncludeInactive tb_batch 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_i-no end_i-no begin_cust end_cust tb_del-zer tb_del-neg 
         tbIncludeInactive tb_batch btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-batch C-Win 
PROCEDURE run-batch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR rd-dest AS INT NO-UNDO.

  {batch/runbatch.i "util\sfgbinbal.r"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------ util/fg-mkbin.p 05/97 JLF */
/* finished goods bin rebuild program                                         */
/* -------------------------------------------------------------------------- */

DEF VAR fcus        LIKE cust.cust-no.
DEF VAR tcus        LIKE fcus               INIT "zzzzzzzz".
DEF VAR fitm        LIKE itemfg.i-no.
DEF VAR titm        LIKE fitm               INIT "zzzzzzzzzzzzzzz".
DEF VAR vzer        AS   LOG                INIT NO.
DEF VAR vneg        AS   LOG                INIT NO.
DEF VAR vInclInact  AS   LOG                INIT NO.


DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
   begin_i-no
   end_i-no
   begin_cust
   end_cust
   tb_del-zer
   tb_del-neg
   tbIncludeInactive.
END.

ASSIGN
 fitm = begin_i-no
 titm = end_i-no
 fcus = begin_cust
 tcus = end_cust
 vzer = tb_del-zer
 vneg = tb_del-neg
 vInclInact = tbIncludeInactive.


SESSION:SET-WAIT-STATE("General").

FOR EACH itemfg
    WHERE itemfg.company    EQ cocode
      AND itemfg.cust-no    GE fcus
      AND itemfg.cust-no    LE tcus
      AND itemfg.i-no       GE fitm
      AND itemfg.i-no       LE titm
      AND TRIM(itemfg.i-no) NE ""
      AND (itemfg.stat EQ "A" OR vInclInact)
    USE-INDEX customer NO-LOCK
    TRANSACTION:

  STATUS DEFAULT "Processing Customer#/FG#: " +
                 TRIM(itemfg.cust-no) + "/" + TRIM(itemfg.i-no).

/*   FIND FIRST reftable                                                    */
/*     WHERE reftable.reftable EQ "FGSTATUS"                                */
/*     AND reftable.company  EQ itemfg.company                              */
/*     AND reftable.loc      EQ ""                                          */
/*     AND reftable.code     EQ itemfg.i-no NO-LOCK NO-ERROR.               */
/*   IF AVAIL reftable AND vInclInact = FALSE AND reftable.code2 = "I" THEN */
/*       NEXT.                                                              */

  RUN fg/fg-mkbin.p (RECID(itemfg)).

  IF vzer THEN
  FOR EACH fg-bin
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ itemfg.i-no
        AND fg-bin.qty     EQ 0:
    DELETE fg-bin.
  END.

  IF vneg THEN
  FOR EACH fg-bin
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ itemfg.i-no
        AND fg-bin.qty     LT 0:

    RUN fg/cre-pchr.p (ROWID(fg-bin), "C", 0, 0).

    DELETE fg-bin.
  END.

  RUN fg/fg-reset.p (RECID(itemfg)).  
END. /* each itemfg */

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("General").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

