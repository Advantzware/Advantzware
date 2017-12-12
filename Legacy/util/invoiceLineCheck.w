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
/* {methods/prgsecur.i} */

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.
DEFINE TEMP-TABLE ttBOLLineProblems
    FIELD company LIKE company.company
    FIELD BolNo   LIKE oe-boll.bol-no COLUMN-LABEL 'BOL#'
    FIELD ItemNo  LIKE oe-boll.i-no FORMAT "X(22)" 
    FIELD ord-no  LIKE oe-boll.ord-no COLUMN-LABEL 'Order #'
    FIELD CustNo  LIKE oe-bolh.cust-no
    FIELD InvDate LIKE inv-head.inv-date    
    .
DEFINE STREAM sReport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fi_file_path btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_file_path 

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

DEFINE VARIABLE fi_file_path AS CHARACTER FORMAT "X(75)" INITIAL "c:~\tmp~\missingInvoiceLines.txt" 
     LABEL "Report Output File" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 11.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_file_path AT ROW 4.48 COL 22 COLON-ALIGNED HELP
          "Enter File Path" WIDGET-ID 6
     btn-process AT ROW 13.67 COL 21
     btn-cancel AT ROW 13.67 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.62 COL 3
     RECT-17 AT ROW 1.86 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 14.33.


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
         TITLE              = "Check for Missing Invoice Lines"
         HEIGHT             = 15.29
         WIDTH              = 90.4
         MAX-HEIGHT         = 23.86
         MAX-WIDTH          = 103.4
         VIRTUAL-HEIGHT     = 23.86
         VIRTUAL-WIDTH      = 103.4
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
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       fi_file_path:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Check for Missing Invoice Lines */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Check for Missing Invoice Lines */
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
ON HELP OF fi_file_path IN FRAME FRAME-A /* Report Output File */
DO:
  DEF VAR v-file-path AS CHAR NO-UNDO.

  SYSTEM-DIALOG GET-DIR v-file-path
     TITLE "Select Archive Files Path".

  fi_file_path:SCREEN-VALUE = v-file-path + "\".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file_path C-Win
ON LEAVE OF fi_file_path IN FRAME FRAME-A /* Report Output File */
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
  DISPLAY fi_file_path 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fi_file_path btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF BUFFER bf-inv-line FOR inv-line.
DEFIN VARIABLE ll AS LOG NO-UNDO.
OUTPUT STREAM sReport TO VALUE(fi_file_path).    

FOR EACH company no-lock,
   EACH inv-head NO-LOCK
    WHERE inv-head.company EQ company.company
    /* test  and inv-head.bol-no eq 777672*/
    ,
    FIRST cust NO-LOCK
    WHERE cust.company EQ inv-head.company
    AND cust.cust-no EQ inv-head.cust-no,
    EACH inv-line OF inv-head NO-LOCK,

    EACH oe-boll WHERE oe-boll.company EQ inv-line.company
    AND oe-boll.b-no    EQ inv-line.b-no                    
    and oe-boll.qty NE 0
    NO-LOCK,
                   
    FIRST oe-bolh OF oe-boll NO-LOCK
    :
        
    FIND FIRST bf-inv-line NO-LOCK 
        WHERE bf-inv-line.company EQ oe-boll.company
        AND bf-inv-line.i-no    EQ oe-boll.i-no
        AND bf-inv-line.po-no   EQ oe-boll.po-no
        AND bf-inv-line.b-no    EQ oe-boll.b-no
        NO-ERROR.
    FIND FIRST ar-invl NO-LOCK
        WHERE ar-invl.company EQ oe-boll.company
        AND ar-invl.i-no    EQ oe-boll.i-no
        AND ar-invl.po-no   EQ oe-boll.po-no
        AND ar-invl.bol-no    EQ oe-boll.bol-no
        and ar-invl.b-no      EQ oe-boll.b-no                 
        USE-INDEX bol-no
        NO-ERROR.
  
    IF NOT AVAIL bf-inv-line AND NOT AVAIL ar-invl THEN 
    DO: 
        FIND FIRST ttBOLLineProblems NO-LOCK
            WHERE ttBOLLineProblems.BolNo EQ oe-boll.bol-no
            AND ttBOLLineProblems.ItemNo EQ oe-boll.i-no
            NO-ERROR.
        IF NOT AVAIL ttBOLLineProblems THEN 
        DO:
            CREATE ttBOLLineProblems.
            ASSIGN 
                ttBOLLineProblems.BolNo   = oe-boll.bol-no
                ttBOLLineProblems.ItemNo  = oe-boll.i-no
                ttBOLLineProblems.CustNo  = oe-bolh.cust-no
                ttBOLLineProblems.InvDate = inv-head.inv-date
                ttBOLLineProblems.company = inv-head.company
                ttBOLLineProblems.ord-no  = oe-boll.ord-no
                .
        END.
    END.
END.

FOR EACH ttBOLLineProblems:
    DISPLAY STREAM sReport ttBOLLineProblems.company ttBOLLineProblems.CustNo ttBOLLineProblems.BolNo ttBOLLineProblems.ord-no ttBOLLineProblems.ItemNo ttBOLLineProblems.InvDate
        WITH stream-io WIDTH 132.
END.

OUTPUT STREAM sReport CLOSE.


    MESSAGE "Report finished. Do you want to view it?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.
 IF ll THEN 
   OS-COMMAND SILENT NOTEPAD VALUE(fi_file_path).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

