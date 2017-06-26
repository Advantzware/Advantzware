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

def var v-process as log no-undo.

DEFINE VARIABLE lRecordFound AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fi_bank-code fi_chknoold fi_chknonew ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_bank-code fi_chknoold fi_chknonew 

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

DEFINE VARIABLE fi_bank-code AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Bank Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_chknonew AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "New Check Number" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_chknoold AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Old Check Number" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_bank-code AT ROW 6.71 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     fi_chknoold AT ROW 8.62 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     fi_chknonew AT ROW 9.81 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date" WIDGET-ID 2
     btn-process AT ROW 14.1 COL 21
     btn-cancel AT ROW 14.1 COL 52
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 15.38.

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
         TITLE              = "Change AP Check Number"
         HEIGHT             = 15.57
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


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Change AP Check Number */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Change AP Check Number */
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
  v-process  = NO.

  MESSAGE "Are you sure you want to change check number "
          fi_chknoold
          " to "
          fi_chknonew
          " for bank "
          fi_bank-code
          "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE v-process.

  IF v-process THEN RUN run-process.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_bank-code C-Win
ON LEAVE OF fi_bank-code IN FRAME FRAME-A /* Bank Code */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_chknonew
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_chknonew C-Win
ON LEAVE OF fi_chknonew IN FRAME FRAME-A /* New Check Number */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_chknoold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_chknoold C-Win
ON LEAVE OF fi_chknoold IN FRAME FRAME-A /* Old Check Number */
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

  IF access-close THEN DO:
    APPLY "close" TO THIS-PROCEDURE.
    RETURN .
  END.

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
  DISPLAY fi_bank-code fi_chknoold fi_chknonew 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fi_bank-code fi_chknoold fi_chknonew btn-process btn-cancel 
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
DEF BUFFER bf-ap-chk FOR ap-chk.
DEF BUFFER bf-ap-dis FOR ap-dis.
DEF BUFFER bf-ap-disl FOR ap-disl.
DEF BUFFER bf-ap-pay FOR ap-pay.
DEF BUFFER bf-ap-payl FOR ap-payl.
DEF BUFFER bf-ap-inv FOR ap-inv.
DEF BUFFER bf-ap-sel FOR ap-sel.
DEF BUFFER bf-aphist FOR aphist.
DEF BUFFER bf-ap-ledger FOR ap-ledger.


SESSION:SET-WAIT-STATE("General").

FIND FIRST bank WHERE bank.company EQ cocode 
    AND bank.bank-code EQ fi_bank-code
    NO-LOCK NO-ERROR.
IF NOT AVAIL bank THEN DO:
    MESSAGE "Invalid Bank Code - " fi_bank-code
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "entry" TO fi_bank-code IN FRAME {&FRAME-NAME}.
    RETURN. 
END.
lRecordFound = NO.
lRecordFound = CAN-FIND(FIRST ap-chk WHERE ap-chk.company EQ cocode
        AND ap-chk.check-no EQ fi_chknonew
        AND ap-chk.bank-code EQ fi_bank-code).
IF NOT lRecordFound THEN
    lRecordFound = CAN-FIND(FIRST ap-dis WHERE ap-dis.company EQ cocode
        AND ap-dis.check-no EQ fi_chknonew
        AND ap-dis.bank-code EQ fi_bank-code).
IF NOT lRecordFound THEN
    lRecordFound = CAN-FIND(FIRST ap-pay WHERE ap-pay.company EQ cocode
        AND ap-pay.check-no EQ fi_chknonew
        AND ap-pay.bank-code EQ fi_bank-code).
IF NOT lRecordFound THEN
    lRecordFound = CAN-FIND(FIRST ap-inv WHERE ap-inv.company EQ cocode
        AND ap-inv.check-no EQ fi_chknonew).
IF NOT lRecordFound THEN
    lRecordFound = CAN-FIND(FIRST aphist WHERE aphist.company EQ cocode
        AND aphist.check-no EQ STRING(fi_chknonew)).
IF NOT lRecordFound THEN
    lRecordFound = CAN-FIND(FIRST ap-sel WHERE ap-sel.company EQ cocode
        AND ap-sel.check-no EQ fi_chknonew
        AND ap-sel.bank-code EQ fi_bank-code).

IF lRecordFound THEN DO:
    MESSAGE "New Check Number already in use for " fi_bank-code
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "entry" TO fi_chknonew IN FRAME {&FRAME-NAME}.
    RETURN. 
END.

FOR EACH ap-chk WHERE ap-chk.company EQ cocode
    AND ap-chk.check-no EQ fi_chknoold
    AND ap-chk.bank-code EQ fi_bank-code 
    NO-LOCK:

    FIND FIRST bf-ap-chk WHERE ROWID(bf-ap-chk) EQ ROWID(ap-chk) EXCLUSIVE-LOCK.
    bf-ap-chk.check-no = fi_chknonew.
    RELEASE bf-ap-chk.

END.

FOR EACH ap-dis WHERE ap-dis.company EQ cocode
    AND ap-dis.check-no EQ fi_chknoold
    AND ap-dis.bank-code EQ fi_bank-code 
    NO-LOCK:

    FIND FIRST bf-ap-dis WHERE ROWID(bf-ap-dis) EQ ROWID(ap-dis) EXCLUSIVE-LOCK.
    bf-ap-dis.check-no = fi_chknonew.
    RELEASE bf-ap-dis.
    FOR EACH ap-disl WHERE ap-disl.company EQ cocode
        AND ap-disl.check-no EQ ap-dis.check-no
        AND ap-disl.d-no EQ ap-dis.d-no
        NO-LOCK:
        FIND FIRST bf-ap-disl WHERE ROWID(bf-ap-disl) EQ ROWID(ap-disl) EXCLUSIVE-LOCK.
        bf-ap-disl.check-no = fi_chknonew.
        RELEASE bf-ap-disl.
    END.
END.

FOR EACH ap-pay WHERE ap-pay.company EQ cocode
    AND ap-pay.check-no EQ fi_chknoold
    AND ap-pay.bank-code EQ fi_bank-code 
    NO-LOCK:

    FIND FIRST bf-ap-pay WHERE ROWID(bf-ap-pay) EQ ROWID(ap-pay) EXCLUSIVE-LOCK.
    bf-ap-pay.check-no = fi_chknonew.
    RELEASE bf-ap-pay.
    FOR EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no
        NO-LOCK:

        FIND FIRST bf-ap-payl WHERE ROWID(bf-ap-payl) EQ ROWID(ap-payl) EXCLUSIVE-LOCK.
         bf-ap-payl.check-no = fi_chknonew.
        RELEASE bf-ap-payl.
    END.
    FOR EACH ap-ledger WHERE ap-ledger.company  EQ ap-pay.company
        AND ap-ledger.vend-no  EQ ap-pay.vend-no
        AND ap-ledger.ref-date EQ ap-pay.check-date
        AND (ap-ledger.refnum EQ "AC" + STRING(fi_chknoold, "999999")
             OR ap-ledger.refnum EQ "CHK# " + string(fi_chknoold) +
                  " CD#" + ap-pay.bank-code
             OR ap-ledger.refnum EQ "VOIDED CHECK" + STRING(fi_chknoold,"zzzzzzz9"))
        NO-LOCK:

        FIND FIRST bf-ap-ledger WHERE ROWID(bf-ap-ledger) EQ ROWID(ap-ledger) EXCLUSIVE-LOCK.
        bf-ap-ledger.refnum = REPLACE(ap-ledger.refnum, STRING(fi_chknoold), STRING(fi_chknonew)).
        RELEASE bf-ap-ledger.
    END.
END.

FOR EACH ap-inv WHERE ap-inv.company EQ cocode
    AND ap-inv.check-no EQ fi_chknoold
    NO-LOCK:

    FIND FIRST bf-ap-inv WHERE ROWID(bf-ap-inv) EQ ROWID(ap-inv) EXCLUSIVE-LOCK.
    bf-ap-inv.check-no = fi_chknonew.
    RELEASE bf-ap-inv.

END.

FOR EACH aphist WHERE aphist.company EQ cocode
    AND aphist.check-no EQ STRING(fi_chknoold)
    NO-LOCK:

    FIND FIRST bf-aphist WHERE ROWID(bf-aphist) EQ ROWID(aphist) EXCLUSIVE-LOCK.
    bf-aphist.check-no = STRING(fi_chknonew).
    RELEASE bf-aphist.

END.

FOR EACH ap-sel WHERE ap-sel.company EQ cocode
    AND ap-sel.check-no EQ fi_chknoold
    AND ap-sel.bank-code EQ fi_bank-code 
    NO-LOCK:

    FIND FIRST bf-ap-sel WHERE ROWID(bf-ap-sel) EQ ROWID(ap-sel) EXCLUSIVE-LOCK.
    bf-ap-sel.check-no = fi_chknonew.
    RELEASE bf-ap-sel.

END.

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

