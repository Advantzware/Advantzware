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
&Scoped-Define ENABLED-OBJECTS from_company from_i-no to_company to_i-no ~
btn-process btn-cancel RECT-17 RECT-18 RECT-19 
&Scoped-Define DISPLAYED-OBJECTS from_company from_c-name from_i-no ~
from_i-name to_company to_c-name to_i-no 

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

DEFINE VARIABLE from_c-name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE from_company AS CHARACTER FORMAT "XXX" INITIAL "001" 
     LABEL "From Company" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE from_i-name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE from_i-no AS CHARACTER FORMAT "X(15)" 
     LABEL "From FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE to_c-name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE to_company AS CHARACTER FORMAT "XXX" INITIAL "001" 
     LABEL "To Company" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE to_i-no AS CHARACTER FORMAT "X(15)" 
     LABEL "To FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 9.05.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 3.57.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     from_company AT ROW 3.14 COL 22 COLON-ALIGNED HELP
          "Enter Company To Copy From"
     from_c-name AT ROW 3.14 COL 44 COLON-ALIGNED NO-LABEL
     from_i-no AT ROW 4.1 COL 22 COLON-ALIGNED HELP
          "Enter Estimate# to Be Copied"
     from_i-name AT ROW 4.1 COL 44 COLON-ALIGNED NO-LABEL
     to_company AT ROW 7.19 COL 22 COLON-ALIGNED HELP
          "Copy To Company"
     to_c-name AT ROW 7.19 COL 44 COLON-ALIGNED NO-LABEL
     to_i-no AT ROW 8.14 COL 22 COLON-ALIGNED HELP
          "Enter Estimate# to Be Copied"
     btn-process AT ROW 11 COL 26
     btn-cancel AT ROW 11 COL 57
     RECT-17 AT ROW 1 COL 1
     RECT-18 AT ROW 2.19 COL 2
     RECT-19 AT ROW 6.48 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .95 AT ROW 1.24 COL 4
          FONT 4
     "C O P Y  F R O M" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 1.95 COL 38
          BGCOLOR 8 FGCOLOR 9 
     "C O P Y  T O" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 6 COL 40
          BGCOLOR 8 FGCOLOR 9 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.6 BY 12.14
         FONT 6.


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
         TITLE              = "Copy FG Item"
         HEIGHT             = 12.24
         WIDTH              = 96.6
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
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


/* SETTINGS FOR FILL-IN from_c-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       from_company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN from_i-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       from_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN to_c-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       to_company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       to_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Copy FG Item */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Copy FG Item */
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
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO from_company.
    RUN valid-company NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-from_i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    APPLY "entry" TO to_company.
    RUN valid-company NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-to_i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) + " " +
          TRIM(from_i-no) + " from Company " + TRIM(from_company) + " to " +
          " Company " + TRIM(TO_company) + "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME from_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_company C-Win
ON HELP OF from_company IN FRAME FRAME-A /* From Company */
DO:
  RUN lookups/company.p.

  IF g_lookup-var NE ""                        AND 
     g_lookup-var NE {&self-name}:SCREEN-VALUE THEN DO:
    {&self-name}:SCREEN-VALUE = g_lookup-var.
    APPLY "value-changed" TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_company C-Win
ON LEAVE OF from_company IN FRAME FRAME-A /* From Company */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-company NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_company C-Win
ON VALUE-CHANGED OF from_company IN FRAME FRAME-A /* From Company */
DO:
  RUN new-company.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME from_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_i-no C-Win
ON HELP OF from_i-no IN FRAME FRAME-A /* From FG Item# */
DO:
  DEF VAR char-val AS cha NO-UNDO.


  RUN windows/l-itemfg.w (from_company:SCREEN-VALUE, "", {&self-name}:SCREEN-VALUE, OUTPUT char-val).

  IF char-val NE "" AND ENTRY(1,char-val) NE TRIM({&self-name}:SCREEN-VALUE) THEN DO:
    {&self-name}:SCREEN-VALUE = ENTRY(1,char-val).
    APPLY "value-changed" TO {&self-name}.
  END.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_i-no C-Win
ON LEAVE OF from_i-no IN FRAME FRAME-A /* From FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-from_i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_i-no C-Win
ON VALUE-CHANGED OF from_i-no IN FRAME FRAME-A /* From FG Item# */
DO:
  RUN new-from_i-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON HELP OF to_company IN FRAME FRAME-A /* To Company */
DO:
  RUN lookups/company.p.

  IF g_lookup-var NE ""                        AND 
     g_lookup-var NE {&self-name}:SCREEN-VALUE THEN DO:
    {&self-name}:SCREEN-VALUE = g_lookup-var.
    APPLY "value-changed" TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON LEAVE OF to_company IN FRAME FRAME-A /* To Company */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-company NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON VALUE-CHANGED OF to_company IN FRAME FRAME-A /* To Company */
DO:
  RUN new-company.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_i-no C-Win
ON LEAVE OF to_i-no IN FRAME FRAME-A /* To FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-to_i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
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

  ASSIGN
   from_company = cocode
   to_company   = cocode.

  RUN enable_UI.

  DO WITH FRAME {&frame-name}:
    DISPLAY from_company to_company.
    {custom/usrprint.i}
    APPLY "entry" TO to_company.
    RUN new-company.
    APPLY "entry" TO from_company.
    RUN new-company.
  END.

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
  DISPLAY from_company from_c-name from_i-no from_i-name to_company to_c-name 
          to_i-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE from_company from_i-no to_company to_i-no btn-process btn-cancel 
         RECT-17 RECT-18 RECT-19 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-company C-Win 
PROCEDURE new-company :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND company WHERE company.company BEGINS FOCUS:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL company THEN DO:
      FOCUS:SCREEN-VALUE = company.company.

      IF FOCUS:NAME EQ "from_company" THEN from_c-name:SCREEN-VALUE = company.NAME.
                                      ELSE to_c-name:SCREEN-VALUE   = company.NAME. 

      APPLY "value-changed" TO from_i-no.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-from_i-no C-Win 
PROCEDURE new-from_i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND itemfg
        WHERE itemfg.company EQ from_company:SCREEN-VALUE
          AND itemfg.i-no    EQ from_i-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN
      ASSIGN
       from_i-name:SCREEN-VALUE = itemfg.i-name
       to_i-no:SCREEN-VALUE     = CAPS(itemfg.i-no).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
  DEF BUFFER b-itemfg        FOR itemfg.
  DEF BUFFER b-cust-part     FOR cust-part.
  DEF BUFFER b-fg-set        FOR fg-set.
  DEF BUFFER b-itemfg-ink    FOR itemfg-ink.
  DEF BUFFER b-itemfg-bom    FOR itemfg-bom.
  DEF BUFFER b-itemfg-loc    FOR itemfg-loc.
  DEF BUFFER b-e-itemfg      FOR e-itemfg.
  DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
  DEF BUFFER b-itemfgdtl     FOR itemfgdtl.
  DEF BUFFER b-ref           FOR reftable.
  DEF BUFFER b-notes         FOR notes.

  DEF VAR lv-rec_key LIKE itemfg.rec_key NO-UNDO.


  SESSION:SET-WAIT-STATE("general").

  FIND FIRST itemfg
      WHERE itemfg.company EQ from_company
        AND itemfg.i-no    EQ from_i-no
      NO-LOCK NO-ERROR.

  lv-rec_key = STRING(TODAY,"99999999") +
               STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
  CREATE rec_key.
  ASSIGN
   rec_key.rec_key    = lv-rec_key
   rec_key.table_name = "ITEMFG".

  CREATE b-itemfg.
  BUFFER-COPY itemfg EXCEPT rec_key TO b-itemfg
  ASSIGN
   b-itemfg.company    = to_company
   b-itemfg.i-no       = to_i-no
   b-itemfg.beg-bal    = 0
   b-itemfg.beg-date   = TODAY
   b-itemfg.q-ptd      = 0
   b-itemfg.q-ord-ytd  = 0
   b-itemfg.u-ord      = 0
   b-itemfg.q-prod-ptd = 0
   b-itemfg.q-prod-ytd = 0
   b-itemfg.u-prod     = 0
   b-itemfg.q-ship-ptd = 0
   b-itemfg.q-ship-ytd = 0
   b-itemfg.u-ship     = 0
   b-itemfg.q-inv-ptd  = 0
   b-itemfg.q-inv-ytd  = 0
   b-itemfg.u-inv      = 0
   b-itemfg.ytd-msf    = 0
   b-itemfg.lyytd-msf  = 0.


  FOR EACH cust-part
      WHERE cust-part.company EQ itemfg.company
        AND cust-part.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE b-cust-part.
    BUFFER-COPY cust-part EXCEPT rec_key TO b-cust-part
    ASSIGN
     b-cust-part.company = b-itemfg.company
     b-cust-part.i-no    = b-itemfg.i-no.
  END.

  FOR EACH fg-set
      WHERE fg-set.company EQ itemfg.company
        AND fg-set.set-no  EQ itemfg.i-no
      NO-LOCK:

    CREATE b-fg-set.
    BUFFER-COPY fg-set EXCEPT rec_key TO b-fg-set
    ASSIGN
     b-fg-set.company = b-itemfg.company
     b-fg-set.set-no  = b-itemfg.i-no.
  END.

  FOR EACH itemfg-ink
      WHERE itemfg-ink.company EQ itemfg.company
        AND itemfg-ink.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE b-itemfg-ink.
    BUFFER-COPY itemfg-ink EXCEPT rec_key TO b-itemfg-ink
    ASSIGN
     b-itemfg-ink.company = b-itemfg.company
     b-itemfg-ink.i-no    = b-itemfg.i-no.


  END.

  FOR EACH itemfg-bom
      WHERE itemfg-bom.company  EQ itemfg.company
        AND itemfg-bom.parent-i EQ itemfg.i-no
      NO-LOCK:
    CREATE b-itemfg-bom.
    BUFFER-COPY itemfg-bom EXCEPT rec_key TO b-itemfg-bom
    ASSIGN
     b-itemfg-bom.company  = b-itemfg.company
     b-itemfg-bom.parent-i = b-itemfg.i-no.
  END.

  FOR EACH itemfg-loc
      WHERE itemfg-loc.company EQ itemfg.company
        AND itemfg-loc.i-no    EQ itemfg.i-no
      NO-LOCK:
    CREATE b-itemfg-loc.
    BUFFER-COPY itemfg-loc EXCEPT rec_key to b-itemfg-loc
    ASSIGN
     b-itemfg-loc.company = b-itemfg.company
     b-itemfg-loc.i-no    = b-itemfg.i-no.

    ASSIGN
     b-itemfg-loc.company    = to_company
     b-itemfg-loc.i-no       = to_i-no
     b-itemfg-loc.q-ptd      = 0
     b-itemfg-loc.q-ord-ytd  = 0
     b-itemfg-loc.q-prod-ptd = 0
     b-itemfg-loc.q-prod-ytd = 0
     b-itemfg-loc.q-ship-ptd = 0
     b-itemfg-loc.q-ship-ytd = 0
     b-itemfg-loc.q-inv-ptd  = 0
     b-itemfg-loc.q-inv-ytd  = 0.     
  END.

  FOR EACH e-itemfg
       WHERE e-itemfg.company EQ itemfg.company
         AND e-itemfg.i-no    EQ itemfg.i-no
      NO-LOCK:
    CREATE b-e-itemfg.
    BUFFER-COPY e-itemfg EXCEPT rec_key TO b-e-itemfg
    ASSIGN
     b-e-itemfg.company = b-itemfg.company
     b-e-itemfg.i-no    = b-itemfg.i-no.
  END.

  FOR EACH e-itemfg-vend
      WHERE e-itemfg-vend.company EQ itemfg.company
        AND e-itemfg-vend.i-no    EQ itemfg.i-no
      NO-LOCK:
    CREATE b-e-itemfg-vend.
    BUFFER-COPY e-itemfg-vend EXCEPT rec_key TO b-e-itemfg-vend
    ASSIGN
     b-e-itemfg-vend.company = b-itemfg.company
     b-e-itemfg-vend.i-no    = b-itemfg.i-no.
  END.

  FOR EACH itemfgdtl
      WHERE itemfgdtl.company EQ itemfg.company
        AND itemfgdtl.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE b-itemfgdtl.
    BUFFER-COPY itemfgdtl EXCEPT rec_key TO b-itemfgdtl
    ASSIGN
     b-itemfgdtl.company = b-itemfg.company
     b-itemfgdtl.i-no    = b-itemfg.i-no.
  END.

  FOR EACH reftable
      WHERE reftable.reftable EQ "FGSTATUS"
        AND reftable.company  EQ itemfg.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ itemfg.i-no
      NO-LOCK:
    CREATE b-ref.
    BUFFER-COPY reftable EXCEPT rec_key TO b-ref
    ASSIGN
     b-ref.company = b-itemfg.company
     b-ref.code    = b-itemfg.i-no.
  END.

  FOR EACH reftable
      WHERE reftable.reftable EQ "itemfg.exempt-disc"
        AND reftable.company  EQ itemfg.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ itemfg.i-no
      NO-LOCK:
    CREATE b-ref.
    BUFFER-COPY reftable EXCEPT rec_key TO b-ref
    ASSIGN
     b-ref.company = b-itemfg.company
     b-ref.code    = b-itemfg.i-no.
  END.

  FOR EACH notes WHERE notes.rec_key EQ itemfg.rec_key NO-LOCK:
    CREATE b-notes.
    BUFFER-COPY notes TO b-notes
    ASSIGN b-notes.rec_key = lv-rec_key.
  END.

  cocode = b-itemfg.company.
  RUN fg/fg-reset.p (RECID(b-itemfg)).

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE("").

  MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-company C-Win 
PROCEDURE valid-company :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR INIT "" NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST company WHERE company.company EQ FOCUS:SCREEN-VALUE) THEN
      lv-msg = "Invalid entry, try help".

    IF lv-msg EQ "" AND FOCUS:NAME EQ "from_company" AND
       NOT CAN-FIND(FIRST itemfg WHERE itemfg.company EQ FOCUS:SCREEN-VALUE) THEN
      lv-msg = "Sorry, no FG Items exist for this company".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO FOCUS.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-from_i-no C-Win 
PROCEDURE valid-from_i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     from_i-no:SCREEN-VALUE = CAPS(from_i-no:SCREEN-VALUE).
     v-msg                  = "".

    FIND FIRST itemfg
        WHERE itemfg.company EQ from_company:SCREEN-VALUE
          AND itemfg.i-no    EQ from_i-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    IF v-msg EQ "" THEN
      IF NOT AVAIL itemfg THEN
        v-msg = TRIM(from_i-no:LABEL) + " doesn't exist, try help...".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO from_i-no.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-to_i-no C-Win 
PROCEDURE valid-to_i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     to_i-no:SCREEN-VALUE = CAPS(to_i-no:SCREEN-VALUE).
     v-msg                = "".

    FIND FIRST itemfg
        WHERE itemfg.company EQ to_company:SCREEN-VALUE
          AND itemfg.i-no    EQ to_i-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    IF v-msg EQ "" THEN
      IF AVAIL itemfg THEN
        v-msg = TRIM(to_i-no:LABEL) + " already exists, please re-enter...".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO to_i-no.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

