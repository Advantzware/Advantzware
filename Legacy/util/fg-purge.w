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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS del_number tb_del-all tb_del-inv btn-process ~
btn-cancel RECT-17 RECT-40 
&Scoped-Define DISPLAYED-OBJECTS del_number tb_del-all tb_del-inv 

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

DEFINE VARIABLE del_number AS CHARACTER FORMAT "X(15)":U 
     LABEL "Enter Item# to be Deleted" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 7.62.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 3.81.

DEFINE VARIABLE tb_del-all AS LOGICAL INITIAL no 
     LABEL "Delete All Finished Good Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .95 NO-UNDO.

DEFINE VARIABLE tb_del-inv AS LOGICAL INITIAL no 
     LABEL "Delete FG even if it has been invoiced?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     del_number AT ROW 6.48 COL 46 COLON-ALIGNED HELP
          "Enter Beginning Item Number to be deleted."
     tb_del-all AT ROW 8.86 COL 27
     tb_del-inv AT ROW 10.76 COL 25
     btn-process AT ROW 16.24 COL 21
     btn-cancel AT ROW 16.24 COL 53
     RECT-17 AT ROW 4.81 COL 1
     RECT-40 AT ROW 6.24 COL 20
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "OR" VIEW-AS TEXT
          SIZE 5 BY .95 AT ROW 7.67 COL 40
          FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-H
     "WARNING: Checking above will delete ALL FG Items!" VIEW-AS TEXT
          SIZE 64 BY 2.62 AT ROW 1 COL 14
          BGCOLOR 12 FGCOLOR 14 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.43
         SIZE 89 BY 2.91
         BGCOLOR 12 FGCOLOR 14 .

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
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge Finished Goods"
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
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE
       FRAME FRAME-H:FRAME = FRAME FRAME-A:HANDLE.

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
/* SETTINGS FOR FRAME FRAME-H
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Finished Goods */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Finished Goods */
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
    ASSIGN
     del_number
     tb_del-all
     tb_del-inv.
  END.

  v-process = NO.

  MESSAGE "Are you sure you want to delete " +
          (IF tb_del-all THEN "ALL FG Items"
                         ELSE ("FG Item: " + TRIM(del_number))) + "?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_del-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_del-all C-Win
ON VALUE-CHANGED OF tb_del-all IN FRAME FRAME-A /* Delete All Finished Good Items? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_del-inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_del-inv C-Win
ON VALUE-CHANGED OF tb_del-inv IN FRAME FRAME-A /* Delete FG even if it has been invoiced? */
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

  FIND ap-ctrl WHERE ap-ctrl.company = gcompany NO-LOCK NO-ERROR.
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
  DISPLAY del_number tb_del-all tb_del-inv 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE del_number tb_del-all tb_del-inv btn-process btn-cancel RECT-17 
         RECT-40 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW FRAME FRAME-H IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-H}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*------------------------------------------------ util/fg-purge.w 01/02 JLF */
/* Purge all inventory records                                               */
/*---------------------------------------------------------------------------*/

SESSION:SET-WAIT-STATE("General").

IF tb_del-all THEN del_number = "".

FOR EACH itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    BEGINS del_number
      AND (itemfg.i-no   EQ del_number OR tb_del-all)
      AND (tb_del-inv OR
           (NOT CAN-FIND(FIRST inv-line WHERE inv-line.company EQ itemfg.company
                                          AND inv-line.i-no    EQ itemfg.i-no) AND
            NOT CAN-FIND(FIRST ar-invl  WHERE ar-invl.company EQ itemfg.company
                                          AND ar-invl.i-no    EQ itemfg.i-no)))
    EXCLUSIVE-LOCK
    TRANSACTION:

  FOR EACH fg-rcpts
      WHERE fg-rcpts.company EQ cocode
        AND fg-rcpts.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE fg-rcpts.
  END.

  FOR EACH fg-rcpth
      WHERE fg-rcpth.company EQ cocode
        AND fg-rcpth.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE fg-rcpth.
  END.

  FOR EACH fg-rctd
      WHERE fg-rctd.company EQ cocode
        AND fg-rctd.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE fg-rctd.
  END.

  FOR EACH fg-hist
      WHERE fg-hist.company EQ cocode
        AND fg-hist.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE fg-hist.
  END.

  FOR EACH fg-act
      WHERE fg-act.company EQ cocode
        AND fg-act.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE fg-act.
  END.

  IF TRIM(itemfg.i-no) NE "" THEN
  FOR EACH fg-bin
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE fg-bin.
  END.

  FOR EACH eb
      WHERE eb.company   EQ cocode
        AND eb.stock-no  EQ itemfg.i-no
        AND eb.stock-no  NE ""
      EXCLUSIVE-LOCK:
    eb.stock-no = "".
  END.

  FOR EACH job-hdr
      WHERE job-hdr.company EQ cocode
        AND job-hdr.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE job-hdr.
  END.

  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ cocode
        AND oe-ordl.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE oe-ordl.
  END.

  FOR EACH oe-rel
      WHERE oe-rel.company EQ cocode
        AND oe-rel.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE oe-rel.
  END.

  FOR EACH oe-rell
      WHERE oe-rell.company EQ cocode
        AND oe-rell.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE oe-rell.
  END.

  FOR EACH oe-boll
      WHERE oe-boll.company EQ cocode
        AND oe-boll.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE oe-boll.
  END.

  FOR EACH inv-line
      WHERE inv-line.company EQ cocode
        AND inv-line.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
    DELETE inv-line.
  END.

  FOR EACH ar-invl
      WHERE ar-invl.company EQ cocode
        AND ar-invl.i-no    EQ itemfg.i-no
        AND ar-invl.i-no    NE ""
      EXCLUSIVE-LOCK:
    DELETE ar-invl.
  END.

  DELETE itemfg.
END.

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

